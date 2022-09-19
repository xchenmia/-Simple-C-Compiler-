//
// Execution engine for simple C programs.  This component 
// executes a simple C program that has passed all syntax 
// and semantic checks. Returns a string denoting successful
// completion/error, along with a list of tuples (name,value)
// representing the final state of memory.
//
// Xiao Chen
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module runner =
  //
  // NOTE: all functions in the module must be indented.
  //


  let findV name value Nmemory = 
    List.map(fun (n,v)->if n = name then (name,value)
                        else (n,v)) Nmemory

   //
  // get_type
  //
  // Given a symbol table, looks up the variable's name and
  // returns its type. If the variable does not exist in the
  // symbol table, "" is returned
  //
  let rec private get_type name ST = 
    match ST with
    | []                      -> ""
    | (n,t)::tl when n = name -> t   // found, return type
    | (n,t)::tl               -> get_type name tl

  let rec private findValue name NM = 
    match NM with
    | []                      -> ""
    | (n,v)::tl when n = name -> v  // found, return type
    | (n,v)::tl               -> get_type name tl

  //
  // matchToken
  //
  let private matchToken expected_token (tokens: string list) =
    // 
    // we already know the syntax is correct, so tokens
    // always match:
    //
    List.tail tokens


    //
  // <expr-value> -> identifier
  //               | int_literal
  //               | real_literal
  //               | str_literal
  //               | true
  //               | false
  //
  let rec private expr_value tokens symboltable =
    let next_token = List.head tokens
    let T2 = matchToken next_token tokens
    //
    if next_token.StartsWith("identifier") then
      let varpos = String.length "identifier:"
      let name = next_token.Substring(varpos)
      let value_type = get_type name symboltable
      (T2, value_type)
    elif next_token.StartsWith("int_literal") then
      (T2, "int")
    elif next_token.StartsWith("real_literal") then
      (T2, "real")
    elif next_token.StartsWith("str_literal") then
      (T2, "str")
    else  // true or false => boolean
      (T2, "bool")



  //
  // <expr-op> -> +
  //            | -
  //            | *
  //            | /
  //            | ^
  //            | <
  //            | <=
  //            | >
  //            | >=
  //            | ==
  //            | !=
  //
  let rec private expr_op tokens  = 
    let next_token = List.head tokens
    matchToken next_token tokens

  let private findRealValue Type value Nmemory = 
    let Value= string value
    // printf"type: %A" Type
    // printfn" testing findRV: %A" Value
    if Value.StartsWith("identifier") then 
      let key = Value.[11..]
      let v = findValue key Nmemory
      v
    else if Type = "int" || Type = "real" then
     if Type = "int" then
       let v = Value.[12..]
       let num = int v
       v
     else
       let v =  Value.[13..]
       v

    else if Value.StartsWith("str_literal") then
      let key = Value.[12..]
      key
    else
      value
      
    


    





  //
  // <expr> -> <expr-value> <expr-op> <expr-value>
  //         | <expr-value>
  //
  let rec private expr tokens symboltable Nmemory = 
    let value:string = List.head tokens
    //printfn"%A testing" value

    // first we have to match expr-value, since both
    // rules start with this:
    //
    let (T2,lhs_type) = expr_value tokens symboltable
    //
    // now let's see if there's more to the expression:
    //
    let next_token = List.head T2
    let T3 = expr_op T2
    let rightvalue:string = List.head T3
    let (T4,rhs_type) = expr_value T3 symboltable

    //
    if next_token = "+"  ||
       next_token = "-"  ||
       next_token = "*"  ||
       next_token = "/"  ||
       next_token = "^"  then
      let Lv = (float (findRealValue lhs_type value Nmemory))
      let Rv =(float (findRealValue rhs_type rightvalue Nmemory))
      if(next_token = "+" ) then
        let returnV = Lv + Rv
        (T4,(string returnV))
      else if (next_token = "-" ) then
        let returnV = Lv - Rv
        (T4,(string returnV))
      else if (next_token = "*" ) then
        let returnV = Lv * Rv
        (T4,(string returnV))
      else if (next_token = "/" ) then
        let returnV = Lv / Rv
        (T4,(string returnV))
      else if (next_token = "^" ) then
        let returnV = Lv ** Rv
        (T4,(string returnV))
      else 
        (T4,"Null")

      //for type check 
    else if next_token = "<"  ||
            next_token = "<=" ||
            next_token = ">"  ||
            next_token = ">=" ||
            next_token = "==" ||
            next_token = "!=" then
      // condition int and real
      if lhs_type = "int" || lhs_type = "real" then
        let Lv = (float (findRealValue lhs_type value Nmemory))
        let Rv =(float (findRealValue rhs_type rightvalue Nmemory))
        if Lv > Rv && next_token = ">"  then
          (T4,"true")
        else if Lv >= Rv && next_token = ">=" then
          (T4,"true")
        else if Lv < Rv && next_token = "<" then
          (T4,"true")
        else if Lv <= Rv && next_token = "<=" then
          (T4,"true")
        else if Lv = Rv && next_token = "==" then
          (T4,"true")
        else if Lv <> Rv && next_token = "!=" then
          (T4,"true")
        else 
          (T4,"false")
      else
        let Lv =  findRealValue lhs_type value Nmemory
        let Rv =  findRealValue rhs_type rightvalue Nmemory
        // printfn "Lv: %A" Lv
        // printfn "Rv: %A" Rv
        if Lv > Rv && next_token = ">" then
          (T4,"true")
        else if Lv >= Rv && next_token = ">=" then
          (T4,"true")
        else if Lv < Rv && next_token = "<" then
          (T4,"true")
        else if Lv <= Rv && next_token = "<=" then
          (T4,"true")
        else if Lv = Rv && next_token = "==" then
          (T4,"true")
        else if Lv <> Rv && next_token = "!=" then
          (T4,"true")
        else 
          (T4,"false")
// expr_value
    else
      // just expr_value, that's it
      if value.StartsWith("identifier") then
        let V = findValue value.[11..] Nmemory
        // printfn" testing %A" V
        (T2,string V)
      else if lhs_type ="real" then
        (T2,value.[13..])
      else if lhs_type ="int" then
       // printfn"%A"  value.[12..]
        (T2,value.[12..])
      else if value ="true" then
         (T2,"true")
      else if value ="false" then
         (T2,"false")
      else 
        (T2,"Null")
    



  //
  // <empty> -> ;
  //
  let rec private empty tokens = 
    matchToken ";" tokens


  //
  // <vardecl> -> int identifier ;
  //            | real identifier ;
  //
  // This is where we check to make sure variables
  // are not being redeclared, and if all is well,
  // we add the var and its type to the symbol table
  // by returning a new symbol table.
  //
  let rec private vardecl tokens = 
    let next_token = List.head tokens
    //
    if next_token = "int" || next_token = "real" then
      let T2 = matchToken next_token tokens
      let T3 = matchToken "identifier" T2
      let T4 = matchToken ";" T3
      T4
    else
      tokens

  // let rec private _rightValue key Value Nmemory memorytabel = 
  //   match Nmemory with 
  //   |[] -> memorytabel
  //   |(Name,Valu):tail when Name = key -> _rightValue key Value tail ((key,Value)::memorytabel)
  //   |(Name,Valu):tail when Name <> key -> _rightValue key Value tail (((Name,Valu)::memorytabel)

  // let private rightValue name value Nmemory = 
  //   _rightValue name value Nmemory []





  //
  // <input> -> cin >> identifier ;
  //
  let rec private input tokens symboltable Nmemory bool = 
    let T2 = matchToken "cin" tokens
    let T3 = matchToken ">>" T2
    let T4 = matchToken "identifier" T3
    let T5 = matchToken ";" T4
    if bool = "true" then
    // get next value and indentifier
      let value:string = List.head T3
      let name = value.[11..]
    // get input value type
      let Type = get_type name symboltable
      let userInput = System.Console.ReadLine()
      if Type = "int" then
        // let M1 = (name, (string(int userInput))):: Nmemory
        let M1 =  findV name (string (int userInput) ) Nmemory
        (T5,M1)
      else
        // let M1 = (name, (string (float userInput))):: Nmemory
        let M1 =  findV name (string (float userInput)) Nmemory
        (T5,M1)
    else
      (T5,Nmemory)
    


   
  


  //
  // <output-value> -> <expr-value>
  //                 | endl
  //
  let rec private output_value tokens symboltable Nmemory bool = 
    let next_token = List.head tokens
    //
    if next_token = "endl" then
      printfn""
      matchToken "endl" tokens    

    else
      if bool = "true" then
        if next_token.StartsWith("str_literal") then
          let str = next_token.[12..]
          printf "%s" str
        else if next_token.StartsWith("int_literal") then
          let str = next_token.[12..]
          printf "%A" (int str)
        else if next_token.StartsWith("real_literal") then
          let str = next_token.[13..]
          printf "%A"  (float str)
        else if next_token.StartsWith("identifier") then
          let key = next_token.[11..]
          let Type = get_type key symboltable
          let num = findValue key Nmemory
          if Type = "real" then
            printf"%A" (float num)
          else
            printf"%s" num
          
        else
          // print true and flase has problems 
          printf "%s"  next_token
        matchToken next_token tokens  
      else
        matchToken next_token tokens  


  //
  // <output> -> cout << <output-value> ;
  //
  let rec private output tokens symboltable Nmemory bool= 
    let T2 = matchToken "cout" tokens
    let T3 = matchToken "<<" T2
    let T4 = output_value T3 symboltable Nmemory bool
    let T5 = matchToken ";" T4
    T5


  //
  // <assignment> -> identifier = <expr> ;
  //
  let rec private assignment tokens symboltable Nmemory bool = 
  // get next value and indentifier
    let value:string = List.head tokens
    // get idntifier name
    let name = value.[11..]
    let T2 = matchToken "identifier" tokens
    let T3 = matchToken "=" T2
    //get value of identifier
    let (T4,num) = expr T3 symboltable Nmemory
    let T5 = matchToken ";" T4
    if num <> "Null" then
      if bool = "true" then
        let M1 = findV name num Nmemory
        (T5,M1)
      else
        (T5,Nmemory)
    else
      (T5,Nmemory)



  //
  // <stmt> -> <empty>
  //         | <vardecl>
  //         | <input>
  //         | <output>
  //         | <assignment>
  //         | <ifstmt>
  //
  let rec private stmt tokens symboltable Nmemory bool = 
    let next_token = List.head tokens 
    //
    if next_token = ";" then
      let T2 = empty tokens
      (T2, Nmemory)
    elif next_token = "int" || next_token = "real" then
      let T2 = vardecl tokens 
      (T2, Nmemory)
    elif next_token = "cin" then
      let (T2,M1) = input tokens symboltable Nmemory bool
      (T2, M1)
    elif next_token = "cout" then
      let T2 = output tokens symboltable Nmemory bool
      (T2, Nmemory)
    elif next_token.StartsWith("identifier") then
      let (T2,M1) = assignment tokens symboltable Nmemory bool
      (T2, M1)
    elif next_token = "if" then
      let (T2,M1) = ifstmt tokens symboltable Nmemory bool
      (T2, M1)
    else
      //
      // this should never happen since syntax is valid,
      // but we need to return something so...
      //
      (tokens, Nmemory)
  //
  // <ifstmt> -> if ( <condition> ) <then-part> <else-part>
  //
  and private  ifstmt tokens symboltable Nmemory Pbool = 
    let T2 = matchToken "if" tokens
    let T3 = matchToken "(" T2

    let (T4,bool) = condition T3 symboltable Nmemory
    
    let T5 = matchToken ")" T4
    if Pbool = "true" then
      let (T6,M1) = then_part T5 symboltable Nmemory bool
      if bool = "true" then
        let Nbool = "false"
        let (T7,M2) = else_part T6 symboltable M1 Nbool
        (T7,M2)
      else 
        let Nbool = "true"
        let (T7,M2) = else_part T6 symboltable M1 Nbool
        (T7,M2)
    
    else
      let (T6,M1) = then_part T5 symboltable Nmemory Pbool // pbool should be false
      let (T7,M2) = else_part T6 symboltable M1 Pbool
      (T7,M2)
   

      
    
  //
  // <condition> -> <expr>
  //
  and private condition tokens symboltable Nmemory = 
   let (T2, bool) = expr tokens symboltable Nmemory
   //printfn "boolTesting: %A" bool
   (T2,bool)
  //
  // <then-part> -> <stmt>
  //
  and private then_part tokens symboltable Nmemory bool = 
      
    let (T2, ST2) = stmt tokens symboltable Nmemory bool
    //
    // NOTE: then-part cannot contain variable declarations,
    // so there's no point in returning a symbol table.
    //
    (T2,ST2)
  //
  // <else-part> -> else <stmt>
  //              | EMPTY
  //
  and private else_part tokens symboltable Nmemory bool = 
    let next_token = List.head tokens
    //
    if next_token = "else" then
      let T2 = matchToken next_token tokens
      let (T3, ST3) = stmt T2 symboltable Nmemory bool
      //
      // NOTE: else-part cannot contain variable declarations,
      // so there's no point in returning a symbol table.
      //
      (T3,ST3)
    else
      // EMPTY, do nothing but return tokens back
      (tokens,Nmemory)


  //
  // <morestmts> -> <stmt> <morestmts>
  //              | EMPTY
  //
  let rec private morestmts tokens symboltable Nmemory = 
    //
    // if the next token denotes the start of a stmt 
    // then process stmt and morestmts, otherwise apply
    // EMPTY
    //
    let next_token = List.head tokens
    //
    if next_token = ";"    ||
       next_token = "int"  ||
       next_token = "real" ||
       next_token = "cin"  ||
       next_token = "cout" ||
       next_token.StartsWith("identifier") ||
       next_token = "if" then
      //
      let (T2, M1) = stmt tokens symboltable Nmemory "true"
      let (T3, M2) = morestmts T2 symboltable M1
      (T3, M2)
    else 
      // EMPTY => do nothing, just return tokens and ST back
      (tokens, Nmemory)





  //
  // <stmts> -> <stmt> <morestmts>
  // 
  let rec private stmts tokens symboltable Nmemory = 
    let (T2, M1) = stmt tokens symboltable Nmemory "true"
  // still have problems 
    let (T3, M2) = morestmts T2 symboltable M1
    (T3, M2)


  let rec private _initializeMemory symboltable memory = 
    match symboltable with
    |[] -> memory
    |(name,Type)::tail -> _initializeMemory tail ((name,"?")::memory)


  let private initializeMemory symboltable =
    _initializeMemory symboltable []
    
  
    

  //
  // <simpleC> -> void main ( ) { <stmts> } $
  //
  let private simpleC tokens symboltable = 
    let Nm = initializeMemory symboltable
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5
    let (T7,memory)= stmts T6 symboltable Nm
    let T8 = matchToken "}" T7
    let T9 = matchToken "$" T8  // $ => EOF, there should be no more tokens
    let Nmemory = List.rev memory
    Nmemory


  //
  // execute tokens symboltable
  //
  // Given a list of tokens and a symbol table, executes
  // the given simple C program. Returns a tuple 
  // (result, memory), where result is a string denoting
  // "done" if the execution was successful, otherwise 
  // a string of the form "execution_error:..." is returned.
  //
  // On success, the final state of memory is returned as a 
  // list of tuples of the form (name, value), e.g. 
  // [("x","123"); ("y","3.14159")]; the order of the names
  // in memory should be in the same order as they appear
  // in the symbol table. On an error, the returned list 
  // is empty [].
  //
  let execute tokens symboltable = 
    try
      let memory = simpleC tokens symboltable
      ("done", memory)
    with 
      | ex -> ("execution_error: " + ex.Message, [])
