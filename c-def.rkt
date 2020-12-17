#lang racket

(provide CGlobalVarDef
         CStructDef
         CFuncDef
         CStmt/LocalVarDef
         CStmt/Assign
         CStmt/Return
         CExpr/ID
         CExpr/Int
         CExpr/Bool
         CExpr/Binary
         CExpr/Call)

(struct CGlobalVarDef
  [type-id name]
  #:transparent)

(struct CStructDef
  [name fields]
  #:transparent)

(struct CFuncDef
  [ret-typ name params statements]
  #:transparent)
(struct CStmt/LocalVarDef [type-id name cexpr] #:transparent)
(struct CStmt/Assign [name cexpr] #:transparent)
(struct CStmt/Return [cexpr] #:transparent)

(struct CExpr/ID [v] #:transparent)
(struct CExpr/Int [v] #:transparent)
(struct CExpr/Bool [v] #:transparent)
(struct CExpr/Binary [op l-expr r-expr] #:transparent)
(struct CExpr/Call [f args] #:transparent)
