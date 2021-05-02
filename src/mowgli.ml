let program = Types.CompoundStatement(
  Types.AssignementStatement("a", Types.OperationExpression(Types.NumberExpression(5), Types.Plus, Types.NumberExpression(3))),
  Types.CompoundStatement(
    Types.AssignementStatement("b",
      Types.EseqExpression(
        Types.PrintStatement[Types.IdentifierExpression("a"); Types.OperationExpression(Types.IdentifierExpression("a"), Minus, Types.NumberExpression(1))],
        Types.OperationExpression(Types.NumberExpression 10, Times, Types.IdentifierExpression("a"))
      )
    ),
    Types.PrintStatement[Types.IdentifierExpression("b")]
  )
);;

let main () =
  Interpretor.interpret program;;