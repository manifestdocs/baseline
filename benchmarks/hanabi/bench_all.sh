#!/usr/bin/env bash
# bench_all.sh — Run all Hanabi benchmarks: Baseline vs C vs Python vs Node.js
#
# Usage:
#   ./benchmarks/hanabi/bench_all.sh
#   ./benchmarks/hanabi/bench_all.sh --runs 3
#   ./benchmarks/hanabi/bench_all.sh --bench nbody

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
BLC="${BLC:-$REPO_DIR/target/release/blc}"

RUNS=3
BENCH_FILTER=""

GREEN='\033[0;32m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BOLD='\033[1m'
DIM='\033[2m'
NC='\033[0m'

while [[ $# -gt 0 ]]; do
  case "$1" in
    --runs) RUNS="$2"; shift 2 ;;
    --bench) BENCH_FILTER="$2"; shift 2 ;;
    -h|--help)
      echo "Usage: $0 [--runs N] [--bench name]"
      echo "  Benchmarks: nbody, binarytrees, fasta, fannkuch, spectralnorm"
      exit 0 ;;
    *) echo "Unknown option: $1" >&2; exit 1 ;;
  esac
done

RESULTS_DIR="$SCRIPT_DIR/results"
mkdir -p "$RESULTS_DIR"
RESULT_FILE="$RESULTS_DIR/hanabi_bench_$(date +%Y%m%d_%H%M%S).md"
TMP_DIR=$(mktemp -d)

cleanup() { rm -rf "$TMP_DIR"; }
trap cleanup EXIT

# Build C references
echo -e "${CYAN}Building C references...${NC}"
C_NBODY="$TMP_DIR/nbody_c"
C_BINTREE="$TMP_DIR/binarytrees_c"
C_FASTA="$TMP_DIR/fasta_c"
clang -O2 "$SCRIPT_DIR/nbody/nbody.c" -lm -o "$C_NBODY"
clang -O2 "$SCRIPT_DIR/binary-trees/binary_trees.c" -o "$C_BINTREE"
clang -O2 "$SCRIPT_DIR/fasta/fasta.c" -o "$C_FASTA"

# Write Python implementations to tmp
cat > "$TMP_DIR/nbody.py" <<'PYEOF'
import sys, math
PI = 3.141592653589793
SOLAR_MASS = 4 * PI * PI
DAYS_PER_YEAR = 365.24

BODIES = [
    [0,0,0,0,0,0,SOLAR_MASS],
    [4.84143144246472090e+00,-1.16032004402742839e+00,-1.03622044471123109e-01,
     1.66007664274403694e-03*DAYS_PER_YEAR,7.69901118419740425e-03*DAYS_PER_YEAR,
     -6.90460016972063023e-05*DAYS_PER_YEAR,9.54791938424326609e-04*SOLAR_MASS],
    [8.34336671824457987e+00,4.12479856412430479e+00,-4.03523417114321381e-01,
     -2.76742510726862411e-03*DAYS_PER_YEAR,4.99852801234917238e-03*DAYS_PER_YEAR,
     2.30417297573763929e-05*DAYS_PER_YEAR,2.85885980666130812e-04*SOLAR_MASS],
    [1.28943695621391310e+01,-1.51111514016986312e+01,-2.23307578892655734e-01,
     2.96460137564761618e-03*DAYS_PER_YEAR,2.37847173959480950e-03*DAYS_PER_YEAR,
     -2.96589568540237556e-05*DAYS_PER_YEAR,4.36624404335156298e-05*SOLAR_MASS],
    [1.53796971148509165e+01,-2.59193146099879641e+01,1.79258772950371181e-01,
     2.68067772490389322e-03*DAYS_PER_YEAR,1.62824170038242295e-03*DAYS_PER_YEAR,
     -9.51592254519715870e-05*DAYS_PER_YEAR,5.15138902046611451e-05*SOLAR_MASS],
]

def offset_momentum():
    px=py=pz=0.0
    for b in BODIES:
        px+=b[3]*b[6]; py+=b[4]*b[6]; pz+=b[5]*b[6]
    BODIES[0][3]=-px/SOLAR_MASS
    BODIES[0][4]=-py/SOLAR_MASS
    BODIES[0][5]=-pz/SOLAR_MASS

def advance(dt):
    for i in range(5):
        bi=BODIES[i]
        for j in range(i+1,5):
            bj=BODIES[j]
            dx=bi[0]-bj[0]; dy=bi[1]-bj[1]; dz=bi[2]-bj[2]
            d2=dx*dx+dy*dy+dz*dz; d=math.sqrt(d2); mag=dt/(d2*d)
            bi[3]-=dx*bj[6]*mag; bi[4]-=dy*bj[6]*mag; bi[5]-=dz*bj[6]*mag
            bj[3]+=dx*bi[6]*mag; bj[4]+=dy*bi[6]*mag; bj[5]+=dz*bi[6]*mag
    for b in BODIES:
        b[0]+=dt*b[3]; b[1]+=dt*b[4]; b[2]+=dt*b[5]

def energy():
    e=0.0
    for i in range(5):
        bi=BODIES[i]
        e+=0.5*bi[6]*(bi[3]*bi[3]+bi[4]*bi[4]+bi[5]*bi[5])
        for j in range(i+1,5):
            bj=BODIES[j]
            dx=bi[0]-bj[0]; dy=bi[1]-bj[1]; dz=bi[2]-bj[2]
            e-=bi[6]*bj[6]/math.sqrt(dx*dx+dy*dy+dz*dz)
    return e

n=int(sys.argv[1]) if len(sys.argv)>1 else 1000
offset_momentum()
print(f"{energy():.9f}")
for _ in range(n): advance(0.01)
print(f"{energy():.9f}")
PYEOF

cat > "$TMP_DIR/binarytrees.py" <<'PYEOF'
import sys

def make(depth):
    if depth<=0: return None
    return (make(depth-1), make(depth-1))

def check(node):
    if node is None: return 1
    return 1+check(node[0])+check(node[1])

n=int(sys.argv[1]) if len(sys.argv)>1 else 21
min_depth=4
max_depth=max(min_depth+2,n)
stretch=max_depth+1
print(f"stretch tree of depth {stretch}\t check: {check(make(stretch))}")
long_lived=make(max_depth)
d=min_depth
while d<=max_depth:
    iters=1<<(max_depth-d+min_depth)
    c=sum(check(make(d)) for _ in range(iters))
    print(f"{iters}\t trees of depth {d}\t check: {c}")
    d+=2
print(f"long lived tree of depth {max_depth}\t check: {check(long_lived)}")
PYEOF

cat > "$TMP_DIR/fasta.py" <<'PYEOF'
import sys
IM=139968; IA=3877; IC=29573
last=42
def gen_random():
    global last; last=(last*IA+IC)%IM; return last
ALU="GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"
IUB=[('a',0.27),('c',0.12),('g',0.12),('t',0.27),('B',0.02),('D',0.02),('H',0.02),('K',0.02),('M',0.02),('N',0.02),('R',0.02),('S',0.02),('V',0.02),('W',0.02),('Y',0.02)]
HS=[('a',0.3029549426680),('c',0.1979883004921),('g',0.1975473066391),('t',0.3015094502008)]
def make_cum(table):
    cp=0.0; r=[]
    for c,p in table: cp+=p; r.append((c,cp))
    return r
def select_random(table):
    r=gen_random()/IM
    for c,p in table:
        if r<p: return c
    return table[-1][0]
def repeat_fasta(header,src,n):
    print(header); pos=0; sl=len(src)
    while n>0:
        nb=min(n,60); line=[]
        for _ in range(nb): line.append(src[pos%sl]); pos+=1
        print(''.join(line)); n-=nb
def random_fasta(header,table,n):
    print(header)
    while n>0:
        nb=min(n,60); print(''.join(select_random(table) for _ in range(nb))); n-=nb
n=int(sys.argv[1]) if len(sys.argv)>1 else 1000
iub=make_cum(IUB); hs=make_cum(HS)
repeat_fasta(">ONE Homo sapiens alu",ALU,n*2)
random_fasta(">TWO IUB ambiguity codes",iub,n*3)
random_fasta(">THREE Homo sapiens frequency",hs,n*5)
PYEOF

# Write Node.js implementations
cat > "$TMP_DIR/nbody.js" <<'JSEOF'
const PI=3.141592653589793,SM=4*PI*PI,DPY=365.24;
const bodies=[
  {x:0,y:0,z:0,vx:0,vy:0,vz:0,m:SM},
  {x:4.84143144246472090,y:-1.16032004402742839,z:-1.03622044471123109e-1,vx:1.66007664274403694e-3*DPY,vy:7.69901118419740425e-3*DPY,vz:-6.90460016972063023e-5*DPY,m:9.54791938424326609e-4*SM},
  {x:8.34336671824457987,y:4.12479856412430479,z:-4.03523417114321381e-1,vx:-2.76742510726862411e-3*DPY,vy:4.99852801234917238e-3*DPY,vz:2.30417297573763929e-5*DPY,m:2.85885980666130812e-4*SM},
  {x:1.28943695621391310e1,y:-1.51111514016986312e1,z:-2.23307578892655734e-1,vx:2.96460137564761618e-3*DPY,vy:2.37847173959480950e-3*DPY,vz:-2.96589568540237556e-5*DPY,m:4.36624404335156298e-5*SM},
  {x:1.53796971148509165e1,y:-2.59193146099879641e1,z:1.79258772950371181e-1,vx:2.68067772490389322e-3*DPY,vy:1.62824170038242295e-3*DPY,vz:-9.51592254519715870e-5*DPY,m:5.15138902046611451e-5*SM},
];
function offset(){let px=0,py=0,pz=0;for(const b of bodies){px+=b.vx*b.m;py+=b.vy*b.m;pz+=b.vz*b.m}bodies[0].vx=-px/SM;bodies[0].vy=-py/SM;bodies[0].vz=-pz/SM}
function advance(dt){for(let i=0;i<5;i++){const bi=bodies[i];for(let j=i+1;j<5;j++){const bj=bodies[j];const dx=bi.x-bj.x,dy=bi.y-bj.y,dz=bi.z-bj.z;const d2=dx*dx+dy*dy+dz*dz,d=Math.sqrt(d2),mag=dt/(d2*d);bi.vx-=dx*bj.m*mag;bi.vy-=dy*bj.m*mag;bi.vz-=dz*bj.m*mag;bj.vx+=dx*bi.m*mag;bj.vy+=dy*bi.m*mag;bj.vz+=dz*bi.m*mag}}for(const b of bodies){b.x+=dt*b.vx;b.y+=dt*b.vy;b.z+=dt*b.vz}}
function energy(){let e=0;for(let i=0;i<5;i++){const bi=bodies[i];e+=.5*bi.m*(bi.vx*bi.vx+bi.vy*bi.vy+bi.vz*bi.vz);for(let j=i+1;j<5;j++){const bj=bodies[j];const dx=bi.x-bj.x,dy=bi.y-bj.y,dz=bi.z-bj.z;e-=bi.m*bj.m/Math.sqrt(dx*dx+dy*dy+dz*dz)}}return e}
const n=parseInt(process.argv[2])||1000;offset();console.log(energy().toFixed(9));for(let i=0;i<n;i++)advance(.01);console.log(energy().toFixed(9));
JSEOF

cat > "$TMP_DIR/binarytrees.js" <<'JSEOF'
function make(d){if(d<=0)return null;return{l:make(d-1),r:make(d-1)}}
function check(n){if(!n)return 1;return 1+check(n.l)+check(n.r)}
const n=parseInt(process.argv[2])||21;
const minD=4,maxD=Math.max(minD+2,n),stretch=maxD+1;
console.log(`stretch tree of depth ${stretch}\t check: ${check(make(stretch))}`);
const longLived=make(maxD);
for(let d=minD;d<=maxD;d+=2){const it=1<<(maxD-d+minD);let c=0;for(let i=0;i<it;i++)c+=check(make(d));console.log(`${it}\t trees of depth ${d}\t check: ${c}`)}
console.log(`long lived tree of depth ${maxD}\t check: ${check(longLived)}`);
JSEOF

cat > "$TMP_DIR/fannkuch.py" <<'PYEOF'
import sys
def fannkuch(n):
    perm=list(range(n)); count=list(range(n)); sign=1; maxflips=0; checksum=0
    while True:
        p=perm[:]
        flips=0
        while p[0]:
            k=p[0]+1; p[:k]=p[:k][::-1]; flips+=1
        maxflips=max(maxflips,flips); checksum+=sign*flips; sign=-sign
        i=1
        while i<n:
            perm.insert(0,perm.pop(i))
            if count[i]: count[i]-=1; break
            count[i]=i; i+=1
        else: break
    return maxflips,checksum
n=int(sys.argv[1]) if len(sys.argv)>1 else 7
mf,cs=fannkuch(n)
print(f"{cs}\nPfannkuchen({n}) = {mf}")
PYEOF

cat > "$TMP_DIR/fannkuch.js" <<'JSEOF'
function fannkuch(n){let perm=Array.from({length:n},(_,i)=>i),count=Array.from({length:n},(_,i)=>i);let sign=1,maxflips=0,checksum=0;
while(true){let p=[...perm],flips=0;while(p[0]){let k=p[0]+1;let rev=p.slice(0,k).reverse();p.splice(0,k,...rev);flips++}
maxflips=Math.max(maxflips,flips);checksum+=sign*flips;sign=-sign;let i=1;
for(;i<n;i++){perm.splice(0,0,perm.splice(i,1)[0]);if(count[i]){count[i]--;break}count[i]=i}
if(i>=n)break}return[maxflips,checksum]}
const n=parseInt(process.argv[2])||7;const[mf,cs]=fannkuch(n);console.log(`${cs}\nPfannkuchen(${n}) = ${mf}`);
JSEOF

cat > "$TMP_DIR/spectralnorm.py" <<'PYEOF'
import sys, math
def a(i,j): return 1.0/((i+j)*(i+j+1)//2+i+1)
def mul_av(n,v):
    return [sum(a(i,j)*v[j] for j in range(n)) for i in range(n)]
def mul_atv(n,v):
    return [sum(a(j,i)*v[j] for j in range(n)) for i in range(n)]
def mul_atav(n,v): return mul_atv(n,mul_av(n,v))
n=int(sys.argv[1]) if len(sys.argv)>1 else 100
u=[1.0]*n; v=[0.0]*n
for _ in range(10): v=mul_atav(n,u); u=mul_atav(n,v)
vbv=sum(u[i]*v[i] for i in range(n))
vv=sum(v[i]*v[i] for i in range(n))
print(f"{math.sqrt(vbv/vv):.9f}")
PYEOF

cat > "$TMP_DIR/spectralnorm.js" <<'JSEOF'
function a(i,j){return 1/((i+j)*(i+j+1)/2+i+1)}
function mulAv(n,v){const r=[];for(let i=0;i<n;i++){let s=0;for(let j=0;j<n;j++)s+=a(i,j)*v[j];r.push(s)}return r}
function mulAtv(n,v){const r=[];for(let i=0;i<n;i++){let s=0;for(let j=0;j<n;j++)s+=a(j,i)*v[j];r.push(s)}return r}
function mulAtAv(n,v){return mulAtv(n,mulAv(n,v))}
const n=parseInt(process.argv[2])||100;
let u=Array(n).fill(1),v=Array(n).fill(0);
for(let i=0;i<10;i++){v=mulAtAv(n,u);u=mulAtAv(n,v)}
let vbv=0,vv=0;for(let i=0;i<n;i++){vbv+=u[i]*v[i];vv+=v[i]*v[i]}
console.log(Math.sqrt(vbv/vv).toFixed(9));
JSEOF

# Timing function: runs command $RUNS times, returns median
time_cmd() {
  local cmd="$1"
  local times=()
  for i in $(seq 1 $RUNS); do
    local t
    t=$( { time eval "$cmd" > /dev/null 2>&1; } 2>&1 | grep real | sed 's/real[[:space:]]*//')
    # Convert to seconds
    local mins secs
    mins=$(echo "$t" | sed 's/m.*//')
    secs=$(echo "$t" | sed 's/.*m//' | sed 's/s//')
    local total
    total=$(python3 -c "print(f'{$mins*60+$secs:.6f}')")
    times+=("$total")
  done
  # Return median
  printf '%s\n' "${times[@]}" | sort -n | sed -n "$((($RUNS+1)/2))p"
}

# Benchmark runner
declare -a BENCH_NAMES=()
declare -a BENCH_RESULTS=()

run_bench() {
  local name="$1"
  local input="$2"
  local c_cmd="$3"
  local bl_cmd="$4"
  local py_cmd="$5"
  local js_cmd="$6"

  if [[ -n "$BENCH_FILTER" && "$name" != *"$BENCH_FILTER"* ]]; then
    return
  fi

  echo ""
  echo -e "${BOLD}${GREEN}━━━ $name (input=$input) ━━━${NC}"

  local c_time bl_time py_time js_time
  echo -ne "  ${BLUE}C -O2${NC}          "
  c_time=$(time_cmd "$c_cmd")
  echo "${c_time}s"

  echo -ne "  ${BLUE}Baseline JIT${NC}   "
  bl_time=$(time_cmd "$bl_cmd")
  echo "${bl_time}s"

  echo -ne "  ${BLUE}Node.js${NC}        "
  js_time=$(time_cmd "$js_cmd")
  echo "${js_time}s"

  echo -ne "  ${BLUE}Python 3.13${NC}    "
  py_time=$(time_cmd "$py_cmd")
  echo "${py_time}s"

  # Compute ratios vs C
  local bl_ratio py_ratio js_ratio
  bl_ratio=$(python3 -c "print(f'{$bl_time/$c_time:.1f}')")
  py_ratio=$(python3 -c "print(f'{$py_time/$c_time:.1f}')")
  js_ratio=$(python3 -c "print(f'{$js_time/$c_time:.1f}')")

  echo ""
  echo -e "  ${DIM}vs C: Baseline=${bl_ratio}x  Node=${js_ratio}x  Python=${py_ratio}x${NC}"

  BENCH_NAMES+=("$name|$input")
  BENCH_RESULTS+=("$c_time|$bl_time|$js_time|$py_time|$bl_ratio|$js_ratio|$py_ratio")
}

echo -e "${BOLD}${GREEN}════════════════════════════════════════${NC}"
echo -e "${BOLD}${GREEN}  Hanabi Benchmark Suite${NC}"
echo -e "${BOLD}${GREEN}  Baseline vs C vs Node.js vs Python${NC}"
echo -e "${BOLD}${GREEN}════════════════════════════════════════${NC}"
echo ""
echo "Runs per benchmark: $RUNS (median)"
echo "System: $(uname -m) $(uname -s) $(uname -r)"

# ── nbody (use optimized version with tuple-let destructuring) ──
run_bench "nbody" "500000" \
  "$C_NBODY 500000" \
  "$BLC run $SCRIPT_DIR/nbody/nbody.bl -- 500000" \
  "python3 $TMP_DIR/nbody.py 500000" \
  "node $TMP_DIR/nbody.js 500000"

run_bench "nbody" "5000000" \
  "$C_NBODY 5000000" \
  "$BLC run $SCRIPT_DIR/nbody/nbody.bl -- 5000000" \
  "python3 $TMP_DIR/nbody.py 5000000" \
  "node $TMP_DIR/nbody.js 5000000"

# ── binarytrees ──
run_bench "binarytrees" "15" \
  "$C_BINTREE 15" \
  "$BLC run $SCRIPT_DIR/binary-trees/binary_trees_opt.bl -- 15" \
  "python3 $TMP_DIR/binarytrees.py 15" \
  "node $TMP_DIR/binarytrees.js 15"

run_bench "binarytrees" "18" \
  "$C_BINTREE 18" \
  "$BLC run $SCRIPT_DIR/binary-trees/binary_trees_opt.bl -- 18" \
  "python3 $TMP_DIR/binarytrees.py 18" \
  "node $TMP_DIR/binarytrees.js 18"

# ── fasta (optimized with native random_fasta_line + cyclic_substring) ──
run_bench "fasta" "2500000" \
  "$C_FASTA 2500000" \
  "$BLC run $SCRIPT_DIR/fasta/fasta_opt.bl -- 2500000" \
  "python3 $TMP_DIR/fasta.py 2500000" \
  "true"

# ── fannkuch-redux (optimized with native count_flips + rotate_left) ──
run_bench "fannkuch" "10" \
  "true" \
  "$BLC run $SCRIPT_DIR/fannkuch-redux/fannkuch_opt.bl -- 10" \
  "python3 $TMP_DIR/fannkuch.py 10" \
  "node $TMP_DIR/fannkuch.js 10"

# ── spectral-norm ──
run_bench "spectralnorm" "500" \
  "true" \
  "$BLC run $SCRIPT_DIR/spectral-norm/spectral_norm.bl -- 500" \
  "python3 $TMP_DIR/spectralnorm.py 500" \
  "node $TMP_DIR/spectralnorm.js 500"

# ── Summary ──
echo ""
echo -e "${BOLD}${GREEN}════════════════════════════════════════════════════════════════════════════${NC}"
echo -e "${BOLD}${GREEN}  Summary (median of $RUNS runs, seconds)${NC}"
echo -e "${BOLD}${GREEN}════════════════════════════════════════════════════════════════════════════${NC}"
echo ""

printf "  ${BOLD}%-22s %10s %10s %10s %10s   %-8s %-8s %-8s${NC}\n" \
  "Benchmark" "C -O2" "Baseline" "Node.js" "Python" "BL/C" "Node/C" "Py/C"
printf "  %-22s %10s %10s %10s %10s   %-8s %-8s %-8s\n" \
  "──────────────────────" "──────────" "──────────" "──────────" "──────────" "────────" "────────" "────────"

for i in "${!BENCH_NAMES[@]}"; do
  IFS='|' read -r name input <<< "${BENCH_NAMES[$i]}"
  IFS='|' read -r ct bt jt pt br jr pr <<< "${BENCH_RESULTS[$i]}"
  printf "  %-22s %10s %10s %10s %10s   %-8s %-8s %-8s\n" \
    "$name($input)" "${ct}s" "${bt}s" "${jt}s" "${pt}s" "${br}x" "${jr}x" "${pr}x"
done

# Write markdown
{
  echo "# Hanabi Benchmark Results"
  echo ""
  echo "**Date:** $(date -u +"%Y-%m-%d %H:%M:%S UTC")"
  echo "**System:** $(uname -m) $(uname -s) $(uname -r)"
  echo "**Runs:** $RUNS (median)"
  echo "**Languages:** C (clang -O2), Baseline (Cranelift JIT), Node.js $(node --version), Python $(python3 --version 2>&1 | awk '{print $2}')"
  echo ""
  echo "| Benchmark | C -O2 | Baseline | Node.js | Python | BL/C | Node/C | Py/C |"
  echo "|-----------|-------|----------|---------|--------|------|--------|------|"
  for i in "${!BENCH_NAMES[@]}"; do
    IFS='|' read -r name input <<< "${BENCH_NAMES[$i]}"
    IFS='|' read -r ct bt jt pt br jr pr <<< "${BENCH_RESULTS[$i]}"
    echo "| $name($input) | ${ct}s | ${bt}s | ${jt}s | ${pt}s | ${br}x | ${jr}x | ${pr}x |"
  done
} > "$RESULT_FILE"

echo ""
echo -e "${CYAN}Results saved to $RESULT_FILE${NC}"
echo -e "${GREEN}Benchmark complete!${NC}"
