; Baseline bracket matching queries

("{" @open "}" @close)
("[" @open "]" @close)
("(" @open ")" @close)

; Lambda pipes
(lambda
  "|" @open
  "|" @close)
