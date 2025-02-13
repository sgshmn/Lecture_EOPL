## LET7_8

기존 LET언어에서 EOPL 책의 Exercise 3.7, Exercise 3.8 요구하는 특징을 확장한 버전이다.

- Exercise 3.7
Extend the language by adding operators for addition, multiplication, and integer quotient.
LET 언어에서는 사칙연산 중 빼기(-)만 구현했다. 더하기, 곱하기, 정수 나눗셈의 몫 연산도 구현하라는 뜻이다.

- Exercise 3.8
Add a numeric equality predicate equal? and numeric order predicates greater? and less? to the set of operations in the defined language.
LET 언어에는 주어진 숫자 x가 0인지 확인하는 zero?(x) 라는 문법만 있다. zero?(x)와 if 문법만으로도 두 숫자를 비교하는 프로그램을 작성할 수 있다. 하지만 두 수를 비교하는 연산을 도입하면 더 쉽게 표현할 수 있다. equal?, greater?, less? 를 구현해보자.


### step1 : 연습문제에서 요구하는 특징을 하나씩 또는 여러 개를 동시에 사용하는 테스트 프로그램들을 작성하기


- step2 : lexer, parser, AST, 인터프리터 확장
- step3 : 기존 LET 테스트케이스가 모두 돌아가는 것 확인 (regression test)
- step4 : 새로 작성한 테스트프로그램 모두 돌아가는 것 확인
