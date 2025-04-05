%{
  open Ast
%}

/* Token declarations */
%token INPUT PRINT
// %token <string> IFILE PFILE
%token LET  /* New token for variable declaration */
%token IF THEN ELSE FOR TO WHILE DO END
%token IPLUS FPLUS IMINUS FMINUS ITIMES FTIMES IDIVIDE FDIVIDE IMODULO FMODULO ABS POWER INT_TO_FLOAT
%token ASSIGN
%token EQ NEQ LT GT LEQ GEQ
%token AND OR NOT XOR
%token ROW_ACCESS
%token TRANSPOSE DET DIMENSION MAGNITUDE ANGLE
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token SEMICOLON COMMA
%token <bool> BOOL_LITERAL
%token <string> ID
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <int * int list> IVECTOR
%token <int * float list> FVECTOR
%token <int * int * int list list> IMATRIX
%token <int * int * float list list> FMATRIX
%token EOF

/* Operator precedence and associativity */
%nonassoc LBRACKET RBRACKET
%left OR XOR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left IPLUS FPLUS
%left IMINUS FMINUS
%left ITIMES FTIMES
%left IDIVIDE FDIVIDE IMODULO FMODULO
%right NOT
%nonassoc TRANSPOSE

/* Starting symbol */
%start program
%type <Ast.program> program

%%

program:
  | stmt_list EOF { Program($1) }
;

stmt_list:
  | /* empty */ { [] }
  | stmt stmt_list { $1 :: $2 }
;

stmt:
  | expr SEMICOLON { ExprStmt($1) }
  | LET ID ASSIGN expr SEMICOLON { DeclareStmt($2, $4) }  /* New variable declaration */
  | ID ASSIGN expr SEMICOLON { AssignStmt($1, $3) }
  | IF expr THEN stmt_block ELSE stmt_block END { IfStmt($2, $4, $6) }
  | IF expr THEN stmt_block END { IfStmt($2, $4, []) }
  | FOR ID ASSIGN expr TO expr DO stmt_block END { ForStmt($2, $4, $6, $8) }
  | WHILE expr DO stmt_block END { WhileStmt($2, $4) }
  | INPUT LPAREN expr RPAREN SEMICOLON { InputStmt($3) }
  | INPUT LPAREN RPAREN SEMICOLON      { InputStmt(Empty) } 
  | PRINT LPAREN expr RPAREN SEMICOLON { PrintStmt($3) }
  | PRINT LPAREN RPAREN SEMICOLON      { PrintStmt(Empty) }
;

stmt_block:
  | stmt { [$1] }
  | LBRACE stmt_list RBRACE { $2 }
;

expr:
  | IVECTOR { 
      let (size, elems) = $1 in
      IVectorLit(size, elems) 
    }
  | FVECTOR { 
      let (size, elems) = $1 in
      FVectorLit(size, elems) 
    }
  | IMATRIX { 
      let (rows, cols, data) = $1 in
      IMatrixLit(rows, cols, data) 
    }
  | FMATRIX { 
      let (rows, cols, data) = $1 in
      FMatrixLit(rows, cols, data) 
    }
  | expr LBRACKET expr RBRACKET { VectorIndex($1, $3) }
  | expr LBRACKET expr COMMA expr RBRACKET { MatrixIndex($1, $3, $5) }
  | LPAREN expr RPAREN { $2 }
  | expr IPLUS expr { BinOp($1, IAdd, $3) }
  | expr FPLUS expr { BinOp($1, FAdd, $3) }
  | expr IMINUS expr { BinOp($1, ISub, $3) }  /* Using /- for integer subtraction */
  | expr FMINUS expr { BinOp($1, FSub, $3) }
  | expr ITIMES expr { BinOp($1, IMul, $3) }
  | expr FTIMES expr { BinOp($1, FMul, $3) }
  | expr IDIVIDE expr { BinOp($1, IDiv, $3) }
  | expr FDIVIDE expr { BinOp($1, FDiv, $3) }
  | expr IMODULO expr { BinOp($1, IMod, $3) }
  | expr FMODULO expr { BinOp($1, FMod, $3) }
  | POWER LPAREN expr COMMA expr RPAREN { BinOp($3, Power, $5) }
  | expr EQ expr { BinOp($1, Equal, $3) }
  | expr NEQ expr { BinOp($1, NotEqual, $3) }
  | expr LT expr { BinOp($1, Less, $3) }
  | expr GT expr { BinOp($1, Greater, $3) }
  | expr LEQ expr { BinOp($1, LessEq, $3) }
  | expr GEQ expr { BinOp($1, GreaterEq, $3) }
  | expr AND expr { BinOp($1, And, $3) }
  | expr OR expr { BinOp($1, Or, $3) }
  | expr XOR expr { BinOp($1, Xor, $3) }
  | ANGLE LPAREN expr COMMA expr RPAREN { BinOp($3, Angle, $5) }
  | NOT expr { UnOp(Not, $2) }
  | IMINUS expr %prec NOT { UnOp(INeg, $2) }  
  | FMINUS expr %prec NOT { UnOp(FNeg, $2) }
  | ABS LPAREN expr RPAREN { UnOp(Abs, $3) }
  | TRANSPOSE LPAREN expr RPAREN { UnOp(Transpose, $3) }
  | DET LPAREN expr RPAREN { UnOp(Det, $3) }
  | DIMENSION LPAREN expr RPAREN { UnOp(Dimension, $3) }
  | MAGNITUDE LPAREN expr RPAREN { UnOp(Magnitude, $3) }
  | INT_TO_FLOAT LPAREN expr RPAREN { UnOp(I2F, $3) }
  | ROW_ACCESS LPAREN expr COMMA expr RPAREN { RowAccess($3, $5) }
  | ID { Var($1) }
  | INT_LITERAL { IntLit($1) }
  | FLOAT_LITERAL { FloatLit($1) }
  | BOOL_LITERAL { BoolLit($1) }
  | STRING_LITERAL { StringLit($1) }
;

%%