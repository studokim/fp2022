  $ ./demoInterpreter.exe <<-EOF
  > echo This is cram tests
  > EOF
  This is cram tests
  Succeeded with retcode 0

Variables and arrays
  $ ./demoInterpreter.exe <<-"EOF"
  > arr=(some This array is)
  > var=123
  > echo $var
  > EOF
  123
  Succeeded with retcode 0

  $ ./demoInterpreter.exe <<-"EOF"
  > echo $((2+3)) $((2<3))
  > EOF
  5 true
  Succeeded with retcode 0

  $ ./demoInterpreter.exe <<-"EOF"
  > arr=(some This array is)
  > echo ${arr[1]} 
  > echo ${arr[3]} 
  > echo ${arr[0]} 
  > echo ${arr[2]} 
  > EOF
  This
  is
  some
  array
  Succeeded with retcode 0

  $ ./demoInterpreter.exe <<-"EOF"
  > a_arr=(elements some This array is with associative 8)
  > echo ${a_arr[2]} ${a_arr[4]} ${a_arr[1]} ${a_arr[6]} ${a_arr[3]} 
  > echo ${a_arr[5]} ${a_arr[7]} ${a_arr[0]}
  > EOF
  This is some associative array
  with 8 elements
  Succeeded with retcode 0

Function (2 variants + recursion)
  $ ./demoInterpreter.exe <<-"EOF"
  > tillzero() {
  >   if(($1==0)); then
  >     echo 0
  >   else
  >     echo $1
  >     tillzero $(($1-1))
  > fi 
  > }
  > 
  > tillzero 10
  > EOF
  10
  9
  8
  7
  6
  5
  4
  3
  2
  1
  0
  Succeeded with retcode 0

  $ ./demoInterpreter.exe <<-"EOF"
  > print() {
  >   echo $1 $2 $3
  > }
  > 
  > print 3 2 1
  > EOF
  3 2 1
  Succeeded with retcode 0

  $ ./demoInterpreter.exe <<-"EOF"
  > 
  > function fib {
  >   a=0
  >   b=1
  >   N=$1
  > 
  >   for((i=1;i<N;i++)) do
  >     echo $a
  >     fn=$((a+b))
  >     a=$b
  >     b=$fn
  >   done
  > }
  > 
  > fib 9
  > EOF
  0
  1
  1
  2
  3
  5
  8
  13
  Succeeded with retcode 0

  $ ./demoInterpreter.exe <<-"EOF"
  > function factorial {
  > n=$1
  > if((n <= 1)); then
  >   echo 1
  > else
  >   last=$(factorial $((n-1)) )
  >   res=$((n*last))
  >   echo $res0
  > fi 
  > }
  > 
  > factorial 10
  > EOF
  3628800
  Succeeded with retcode 0

Expansions

  $ ./demoInterpreter.exe <<-"EOF"
  > echo This is Parameter expansion
  > var=somevalue
  > echo length result is ${#var}
  > echo offset result is ${var:4}
  > echo two offsets result is ${var:3:4}
  > var=abcabcabc
  > emptyvar=''
  > echo ReplFirst result is ${var/abc/hi}
  > echo ReplAll result is ${var//abc/hi}
  > echo ReplBeg result is ${var/#abc/hi} but ${var/#bc/hi}
  > echo ReplEnd result is ${var/%abc/hi} but ${var/%ab/hi}
  > echo RemShortFromBeg result is ${var#*} and ${var#[a-z]}
  > echo RemLargFromBeg result is ${var##*} and ${var##[a-z]} 
  > echo RemShortFromEnd result is ${var%*} and ${var%[a-z]}
  > echo RemLargFromEnd result is ${var%%*} and ${var%%[a-z]} 
  > echo UseDefValue result is ${somevar:-DEF} and ${var:-DEF} and ${emptyvar:-DEF} 
  > echo SetDefValue result is ${somevar:=value} and ${var:=value} and ${emptyvar:=value}
  > echo UseAlterValue result is ${somevar:+newvalue} and ${var:+newvalue} and ${emptyvar:+newvalue}
  > EOF
  This is Parameter expansion
  length result is 9
  offset result is value
  two offsets result is eval
  ReplFirst result is hiabcabc
  ReplAll result is hihihi
  ReplBeg result is hiabcabc but abcabcabc
  ReplEnd result is abcabchi but abcabcabc
  RemShortFromBeg result is abcabcabc and bcabcabc
  RemLargFromBeg result is  and bcabcabc
  RemShortFromEnd result is abcabcabc and abcabcab
  RemLargFromEnd result is  and abcabcab
  UseDefValue result is DEF and abcabcabc and 
  SetDefValue result is value and abcabcabc and value
  UseAlterValue result is is and newvalue and 
  Succeeded with retcode 0

  $ ./demoInterpreter.exe <<-"EOF"
  > var='1 2 3 $((2+3))'
  > echo $var
  1 2 3 $((2+3))
  Succeeded with retcode 0

  $ ./demoInterpreter.exe <<-"EOF"
  > var="1 2 3 $((2+3))"
  > echo $var
  1 2 3 5
  Succeeded with retcode 0

  $ ./demoInterpreter.exe <<-"EOF"
  > echo a{d,c,b}e
  > EOF
  ade
   ace
   abe
   
  Succeeded with retcode 0

  $ ./demoInterpreter.exe <<-"EOF"
  > echo `echo 1 2 3`
  > echo $(echo 3 2 1)
  > EOF
  1 2 3
  3 2 1
  Succeeded with retcode 0

  $ ./demoInterpreter.exe <<-"EOF"
  > echo $((2+3)) $((2<3))
  > EOF
  5 true
  Succeeded with retcode 0

Compound
  $ ./demoInterpreter.exe <<-"EOF"
  > var=true
  > if (($var)); then
  >   echo var is true
  > else
  >   echo var is false
  > fi
  > 
  > var=false
  > if (($var)); then
  >   echo var is true
  > else
  >   echo var is false
  > fi  
  > EOF
  var is true
  var is false
  Succeeded with retcode 0

  $ ./demoInterpreter.exe <<-"EOF"
  > for((i=0;i<3;i++)) do
  >   echo $i
  > done
  > EOF
  0
  1
  2
  Succeeded with retcode 0

  $ ./demoInterpreter.exe <<-"EOF"
  > i=0
  > while((i<3)) do
  >   echo $i
  >   i=$((i+1))
  > done
  > EOF
  0
  1
  2
  Succeeded with retcode 0

  $ ./demoInterpreter.exe <<-"EOF"
  > for i in one two 3 do
  >  echo $i
  > done
  one
  two
  3
  Succeeded with retcode 0

  $ ./demoInterpreter.exe <<-"EOF"
  > country=Romania
  > 
  > case $country in
  > Lithuania)
  >   echo Lithuanian
  > ;;
  > 
  > Moldova | Romania)
  >   echo Romanian
  > ;;
  > 
  > Italy | Switzerland)
  >   echo Italian
  > ;;
  > esac
  > EOF
  Romanian
  Succeeded with retcode 0

Pipelines 

  $ ./demoInterpreter.exe <<-"EOF"
  > echo 1 2 3 | cat
  > EOF
  1 2 3
  Succeeded with retcode 0

  $ ./demoInterpreter.exe <<-"EOF"
  > var=false
  > $((var)) && echo 1 2 3 | cat
  > var=true
  > $((var)) && echo 3 2 1 | cat
  > EOF
  3 2 1
  Succeeded with retcode 0

  $ ./demoInterpreter.exe <<-"EOF"
  > var=false
  > $((var)) || echo 1 2 3 | cat
  > var=true
  > $((var)) || echo 3 2 1 | cat
  > EOF
  1 2 3
  Succeeded with retcode 0

Redirect
  $ ./demoInterpreter.exe <<-"EOF"
  > echo 1 2 3 > a.txt
  > cat < a.txt
  > EOF
  1 2 3
  Succeeded with retcode 0

  $ ./demoInterpreter.exe <<-"EOF"
  > echo 1 2 3 > a.txt
  > echo 3 2 1 >> a.txt
  > cat < a.txt
  > EOF
  1 2 3
  3 2 1
  Succeeded with retcode 0
