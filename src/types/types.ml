type id = string;;

type binop =
| Plus
| Minus
| Times
| Division;;

type statement =
| CompoundStatement of statement * statement
| AssignementStatement of id * expression
| PrintStatement of expression list

and expression =
| IdentifierExpression of id
| NumberExpression of int
| OperationExpression of expression * binop * expression
| EseqExpression of statement * expression;;
