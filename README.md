# GoAwayBigCovid

## Servidor Front-end

### Pedidos

#### Porta

`8001`

#### Formato

Primeiro pedido depois de estabelecer a ligação:

| Pedido | Sintaxe | Resultado |
| --- | --- | --- |
| create account | `"ca <username> <password> <nº distrito> <locationX> <locationY>"` | <code>"ok" &#124; "error user_exists"</code> |
| login | `"li <username> <password>"` | <code>"ok" &#124; "error invalid" &#124; "error already_logged_in"</code> |
| \<outro\> | - | `"error invalid_request"` |

Restantes pedidos:

| Pedido | Sintaxe | Resultado |
| --- | --- | --- |
| logout | `"lo"` | `"ok"` |
| update location | `"ul <locationX> <locationY>"` | <code>"ok" &#124; "error no_user"</code> |
| users in location | `"us <locationX> <locationY>"` | `"<number>"` |
| add infected user | `"ai"` | <code>"ok" &#124; "error no_user"</code> |
| \<outro\> | - | `"error invalid_request"` |

( i ) Nenhum dos argumentos pode conter espaços

### Notificações Privadas

#### Porta

`8002`

#### Formato

`"<nº distrito> <id utilizador> <mensagem>"`

( i ) `<mensagem>` pode conter espaços

## Servidor Distrital

### Distritos

Os números dos distritos são os seguintes, tendo sempre 2 dígitos:

`01`: Aveiro\
`02`: Beja\
`03`: Braga\
`04`: Bragança\
`05`: Castelo Branco\
`06`: Coimbra\
`07`: Évora\
`08`: Faro\
`09`: Guarda\
`10`: Leiria\
`11`: Lisboa\
`12`: Portalegre\
`13`: Porto\
`14`: Santarém\
`15`: Setúbal\
`16`: Viana do Castelo\
`17`: Vila Real\
`18`: Viseu

### Threads

`1`: Thread Request/Reply\
`2`: Thread Notificações Públicas\
`3`: Thread Notificações Privadas

### Formato das Portas

`7|nºdistrito|nºthread`

### Formato dos Pedidos

| Pedido | Sintaxe | Resultado |
| --- | --- | --- |
| new user | `"nu <locationX> <locationY>"` | `"<id>"` |
| update location | `"ul <id> <locationX> <locationY>"` | <code>"ok" &#124; "error no_user"</code> |
| users in location | `"us <locationX> <locationY>"` | `"<number>"` |
| add infected user | `"ai <id>"` | <code>"ok" &#124; "error no_user"</code> |
| \<outro\> | - | `"error invalid_request"` |
