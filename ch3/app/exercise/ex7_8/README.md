## LET7_8

기존 LET언어에서 EOPL 책의 Exercise 3.7, Exercise 3.8 요구하는 특징을 확장한 버전이다.

- Exercise 3.7
Extend the language by adding operators for addition, multiplication, and integer quotient.
LET 언어에서는 사칙연산 중 빼기(-)만 구현했다. 더하기, 곱하기, 정수 나눗셈의 몫 연산도 구현하라는 뜻이다.

- Exercise 3.8
Add a numeric equality predicate equal? and numeric order predicates greater? and less? to the set of operations in the defined language.
LET 언어에는 주어진 숫자 x가 0인지 확인하는 zero?(x) 라는 문법만 있다. zero?(x)와 if 문법만으로도 두 숫자를 비교하는 프로그램을 작성할 수 있다. 하지만 두 수를 비교하는 연산을 도입하면 더 쉽게 표현할 수 있다. equal?, greater?, less? 를 구현해보자.

### step0 : LET7_8 폴더 만들기
LET7_8은 LET의 확장이기 때문에 LET을 복제해서 일부를 수정하면 된다
1. ch3/pakage.yaml 파일을 수정한다
    - executables의 letlang-exe를 복사 붙여넣기 한다
    - 이름을 let7_8lang-exe로 바꾼다
    - source-dirs의 내용을을 app/exercise/ex7_8로 바꾼다
    - tests의 letlang-test를 복사 붙여넣기 한다
    - 이름을 let7_8lang-test로 바꾼다
    - source-dirs의 내용을 test/exercise/ex7_8, app/exercise/ex7_8로 바꾼다
2. ch3/app/exercise 폴더를 만든다
3. ch3/app/letlang을 복사해서 ch3/app/exercise 에 붙여넣고 이름을 ex7_8로 바꾼다
    - LetLang.lhs를 Let7_8Lang.lhs로 이름과 내용을 바꾼다
4. ch3/test/exercise 폴더를 만든다
5. ch3/test/letlang을 복사해서 ch3/app/exercise 에 붙여넣고 이름을 ex7_8로 바꾼다
    - ch3/app/exercise/ex7_8/Spec.hs 파일에서 이름, 경로를 바꾼다
        - describe \"letlang\" \$ do -> describe \"let7_8lang\" \$ do
        - let atdir f = \"./app/let/examples/\" ++ f -> let atdir f = \"./app/exercise/ex7_8/examples/\" ++ f
6. 실행  
    - 쉘(window powershell, 명령 프롬프트 등)을 켜고 ch3 에서 다음을 실행해보자
    - stack build
    - stack test ch3:test:let7_8lang-test
    - 이 두개를 실행했을 때 에러가 나오지 않아야 한다
    - 아직까지는 LET 언어를 복제하고 이름만 바꿨기 때문이다


### step1 : 연습문제에서 요구하는 특징을 하나씩 또는 여러 개를 동시에 사용하는 테스트 프로그램들을 작성하기
LET언어의 뺄셈 연산의 문법은 다음과 같다  
-(a,b)  
마찬가지로 덧셈, 곱셈, 몫 연산도 만들면  
+(a,b)  
*(a,b)  
/(a,b)  
비교 연산자도 만들자  
equal?(a,b)  
greater?(a,b)  
less?(a,b)  
  
위 연산을 활용한 테스트 프로그램을 ch3/app/exercise/ex7_8/examples에 let7_8확장자로 만든다  
폴더에 가면 simple_arith_2.let7_8 ... 등을 확인할 수 있다  
ch3/app/exercise/ex7_8/Spec.hs에 추가한 테스트케이스와 답을 넣는다


### step2 : lexer, parser, AST, 인터프리터 확장
1. Token.hs  
data Token에 ADD, MUL, QUO, ISEQUAL, ISGREATER, ISLESS를 추가한다  
tokenStrList에 추가한 Token에 맞는 문법을 입력한다
    - (ADD, "+")
    - (ISEQUAL, "equal?")
    - 이런 식으로
  
2. Lexer.hs  
lexerSpecList에 사칙연산, 비교연산을 추가한다  
    - ("\\+"     , mkFn ADD)
    - ("equal\\?" , mkFn ISEQUAL)
    - 이런 식으로

3. Expr.hs
data Exp에 사칙연산, 비교연산 Exp를 추가한다  
Add_Exp, Mul_Exp, Quot_Exp, IsEqual_Exp, IsGreater_Exp, IsLess_Exp  


4. Parser.hs  
parserSpecList 에 사칙연산, 비교연산의 rule을 추가한다  
    - rule "Expression -> + ( Expression , Expression )"
    -   (\rhs -> return $ Add_Exp (get rhs 3) (get rhs 5)),
    - \+ ( Expression , Expression ) 도 Expression이 될 수 있다는 생산규칙 추가
    - 이 생산규칙에 맞는 Expression이 들어오면 Add_Exp에 넣는데
    - \"+ ( a , b )\" 식의 
    - 3번째 인자(get rhs 3, a) Expression,
    - 5번째 인자(get rhs 5, b) Expression을
    - Add_Exp에 넣어준다
    - 같은 방법으로 Mul_Exp, Quot_Exp, IsEqual_Exp, IsGreater_Exp, IsLess_Exp과 관련된 생산규칙을 만든다
아래 letlang이 들어간 문자열을 let7_8lang으로 바꾼다

5. Interp.hs, Env.hs  
추가한 Exp의 value_of 함수를 만든다  
LET7_8 에서는 Env를 수정하지 않아도 된다  


### step3,4 : 기존 LET 테스트케이스가 모두 돌아가는 것 확인 (regression test), 새로 작성한 테스트프로그램 모두 돌아가는 것 확인
쉘을 켜고 ch3/ 에서 다음 명령어 입력  
stack build     
stack run let7_8lang-exe .\app\exercise\ex7_8\examples\테스트_프로그램   
stack test ch3:test:let7_8lang-test  
  
stack run은 에러가 없으면 성공
stack test ... 은 에러가 없고 failure가 없어야 성공
