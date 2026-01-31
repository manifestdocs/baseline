; Rocket bracket matching queries

("{" @open "}" @close)
("[" @open "]" @close)
("(" @open ")" @close)

; Lambda pipes
(lambda_expression
  "|" @open
  "|" @close)
