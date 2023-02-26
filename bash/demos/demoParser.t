  $ ./demoParser.exe <<-EOF
  > echo 1 2 3
  > echo 2 3 4;;
  > echo
  > echo echo;;
  > EOF
  (Declarations
     [(Pipeline
         (Pipe
            [(SimpleCommand ("echo",
                [(Const (String "1")); (Const (String "2"));
                  (Const (String "3"))]
                ))
              ]));
       (Pipeline
          (Pipe
             [(SimpleCommand ("echo",
                 [(Const (String "2")); (Const (String "3"));
                   (Const (String "4"))]
                 ))
               ]));
       (Pipeline (Pipe [(SimpleCommand ("echo", []))]));
       (Pipeline (Pipe [(SimpleCommand ("echo", [(Const (String "echo"))]))]))])

  $ ./demoParser.exe <<-EOF
  > function myfunc {
  > echo hello
  > var=true
  > if((var)); then 
  >   somecmd1 with args
  > else 
  >   somecmd2 with 3 args
  > fi }
  > EOF
  (Declarations
     [(Funcn
         ("myfunc",
          [(SimpleCommand ("echo", [(Const (String "hello"))]));
            (VarAssignment (SimpleVar (("var", "0"), (Const (String "true")))));
            (Compound
               (IfElse ((Var ("var", "0")),
                  [(SimpleCommand ("somecmd1",
                      [(Const (String "with")); (Const (String "args"))]))
                    ],
                  (Some [(SimpleCommand ("somecmd2",
                            [(Const (String "with")); (Const (String "3"));
                              (Const (String "args"))]
                            ))
                          ])
                  )))
            ]))
       ])

