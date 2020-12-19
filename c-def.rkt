#lang racket

(provide (struct-out CGlobalVarDef)
         (struct-out CStructDef)
         (struct-out CFuncDef)
         (struct-out CStmt/LocalVarDef)
         (struct-out CStmt/Assign)
         (struct-out CStmt/Return)
         (struct-out CExpr/ID)
         (struct-out CExpr/Int)
         (struct-out CExpr/Bool)
         (struct-out CExpr/Binary)
         (struct-out CExpr/Call))

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
