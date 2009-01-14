$VERBOSE = nil

require File.dirname(__FILE__) + "/../lib/heist"
require "test/unit"

Class.new(Test::Unit::TestCase) do
  def setup
    return @@env if defined?(@@env)
    @@env = Heist::Runtime.new # (:order => Heist::EAGER)
    puts "\nType of if() function: #{ @@env["if"].class }"
    puts "Application mode: #{ Heist::ORDERS[@@env.order] }\n\n"
  end
  
  def test_numbers
    assert_equal 486, @@env.eval("486")
  end
  
  def test_arithmetic
    assert_equal 486,  @@env.eval("(+ 137 349)")
    assert_equal 666,  @@env.eval("(- 1000 334)")
    assert_equal 495,  @@env.eval("(* 5 99)")
    assert_equal 2,    @@env.eval("(/ 10 5)")
    assert_equal 12.7, @@env.eval("(+ 2.7 10)")
    assert_equal 75,   @@env.eval("(+ 21 35 12 7)")
    assert_equal 1200, @@env.eval("(* 25 4 12)")
    assert_equal 19,   @@env.eval("(+ (* 3 5) (- 10 6))")
    assert_equal 57,   @@env.eval("(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))")
  end
  
  def test_define_values
    @@env.eval("(define size 2)")
    assert_equal 2, @@env.eval("size")
    
    @@env.eval <<-CODE
      (define pi 3.14159)
      (define radius 10)
    CODE
    assert_equal 314.159, @@env.eval("(* pi (* radius radius))")
    
    @@env.eval("(define circumference (* 2 pi radius))")
    assert_equal 62.8318, @@env.eval("circumference")
  end
  
  def test_define_functions
    @@env.eval("(define (square x) (* x x))")
    assert_equal 441, @@env.eval("(square 21)")
    assert_equal 49,  @@env.eval("(square (+ 2 5))")
    assert_equal 81,  @@env.eval("(square (square 3))")
    
    @@env.eval <<-CODE
      (define (sum-of-squares x y)
        (+ (square x) (square y)))
    CODE
    assert_equal 25, @@env.eval("(sum-of-squares 3 4)")
    
    @@env.eval <<-CODE
      (define (f a)
        (sum-of-squares (+ a 1) (* a 2)))
    CODE
    assert_equal 136, @@env.eval("(f 5)")
    assert_equal @@env.eval("(f 5)"),
        @@env.eval("((lambda (a) ((lambda (x y) (+ (square x) (square y))) (+ a 1) (* a 2))) 5)")
  end
  
  def test_closures
    result = @@env.eval <<-CODE
      (define (add n)
        (lambda (x) (+ x n)))
      
      (define add4 (add 4))
      (define add7 (add 7))
      
      (add4 11)
    CODE
    assert_equal 15, result
    
    @@env.eval <<-CODE
      (define (weird x y)
        (begin
          (define (+ x y)
            (* x y))
          (+ x y)))
    CODE
    assert_equal 7,  @@env.eval("(+ 3 4)")
    assert_equal 12,  @@env.eval("(weird 3 4)")
  end
  
  def test_cond
    @@env.eval <<-CODE
      (define (abs x)
        (cond ((> x 0) x)
              ((= x 0) 0)
              ((< x 0) (- x))))
    CODE
    assert_equal 4,  @@env.eval("(abs 4)")
    assert_equal 0,  @@env.eval("(abs 0)")
    assert_equal 13, @@env.eval("(abs -13)")
  end
  
  def test_else
    @@env.eval <<-CODE
      (define (abs x)
        (cond ((< x 0) (- x))
              (else x)))
    CODE
    assert_equal 4,  @@env.eval("(abs 4)")
    assert_equal 0,  @@env.eval("(abs 0)")
    assert_equal 13, @@env.eval("(abs -13)")
  end
  
  def test_if
    @@env.eval <<-CODE
      (define (abs x)
        (if (< x 0)
            (- x)
            x))
    CODE
    assert_equal 4,  @@env.eval("(abs 4)")
    assert_equal 0,  @@env.eval("(abs 0)")
    assert_equal 13, @@env.eval("(abs -13)")
    
    @@env.eval <<-CODE
      (define (a-plus-abs-b a b)
        ((if (> b 0) + -) a b))
    CODE
    
    assert_equal 7, @@env.eval("(a-plus-abs-b 3 4)")
    assert_equal 11, @@env.eval("(a-plus-abs-b 3 (- 8))")
  end
  
  def test_if_should_not_infinitely_recurse
    @@env.eval <<-CODE
      (define (fact x)
        (if (= x 0) 1
            (* x
            (fact (- x 1)))))
    CODE
    assert_equal 720, @@env.eval("(fact 6)")
  end
  
  def test_and_or
    @@env.eval("(define x 7)")
    assert_equal true, @@env.eval("(and (> x 5) (<= x 10))")
    assert_equal true, @@env.eval("(or (>= x 5) (< x 3))")
  end
  
  def test_birds
    return unless @@env.lazy?
    @@env.eval('(load "birds")')
    
    @@env.eval <<-CODE
      (define factorial (Y
        (lambda (rec)
          (lambda (x)
            (if (= x 0) 1 (* x (rec (- x 1))))))))
    CODE
    assert_equal (1..6).inject { |a,b| a*b },
                 @@env.eval("(factorial 6)")
    
    assert_equal 45, @@env.eval("((K 45) 6)")
  end
end

