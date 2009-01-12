require File.dirname(__FILE__) + "/../lib/heist"
require "test/unit"

class HeistTest < Test::Unit::TestCase
  def setup
    @env = Heist::Runtime.new
  end
  
  def test_numbers
    assert_equal 486, Heist.eval("486")
  end
  
  def test_arithmetic
    assert_equal 486,  Heist.eval("(+ 137 349)")
    assert_equal 666,  Heist.eval("(- 1000 334)")
    assert_equal 495,  Heist.eval("(* 5 99)")
    assert_equal 2,    Heist.eval("(/ 10 5)")
    assert_equal 12.7, Heist.eval("(+ 2.7 10)")
    assert_equal 75,   Heist.eval("(+ 21 35 12 7)")
    assert_equal 1200, Heist.eval("(* 25 4 12)")
    assert_equal 19,   Heist.eval("(+ (* 3 5) (- 10 6))")
    assert_equal 57,   Heist.eval("(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))")
  end
  
  def test_define_values
    @env.eval("(define size 2)")
    assert_equal 2, @env.eval("size")
    
    @env.eval <<-CODE
      (define pi 3.14159)
      (define radius 10)
    CODE
    assert_equal 314.159, @env.eval("(* pi (* radius radius))")
    
    @env.eval("(define circumference (* 2 pi radius))")
    assert_equal 62.8318, @env.eval("circumference")
  end
  
  def test_define_functions
    @env.eval("(define (square x) (* x x))")
    assert_equal 441, @env.eval("(square 21)")
    assert_equal 49,  @env.eval("(square (+ 2 5))")
    assert_equal 81,  @env.eval("(square (square 3))")
    
    @env.eval <<-CODE
      (define (sum-of-squares x y)
        (+ (square x) (square y)))
    CODE
    assert_equal 25, @env.eval("(sum-of-squares 3 4)")
    
    @env.eval <<-CODE
      (define (f a)
        (sum-of-squares (+ a 1) (* a 2)))
    CODE
    assert_equal 136, @env.eval("(f 5)")
    assert_equal @env.eval("(f 5)"),
        @env.eval("((lambda (a) ((lambda (x y) (+ (square x) (square y))) (+ a 1) (* a 2))) 5)")
  end
  
  def test_cond
    @env.eval <<-CODE
      (define (abs x)
        (cond ((> x 0) x)
              ((= x 0) 0)
              ((< x 0) (- x))))
    CODE
    assert_equal 4,  @env.eval("(abs 4)")
    assert_equal 0,  @env.eval("(abs 0)")
    assert_equal 13, @env.eval("(abs -13)")
  end
  
  def test_else
    @env.eval <<-CODE
      (define (abs x)
        (cond ((< x 0) (- x))
              (else x)))
    CODE
    assert_equal 4,  @env.eval("(abs 4)")
    assert_equal 0,  @env.eval("(abs 0)")
    assert_equal 13, @env.eval("(abs -13)")
  end
  
  def test_if
    @env.eval <<-CODE
      (define (abs x)
        (if (< x 0)
            (- x)
            x))
    CODE
    assert_equal 4,  @env.eval("(abs 4)")
    assert_equal 0,  @env.eval("(abs 0)")
    assert_equal 13, @env.eval("(abs -13)")
  end
  
  def test_and_or
    @env.eval("(define x 7)")
    assert_equal true, @env.eval("(and (> x 5) (<= x 10))")
    assert_equal true, @env.eval("(or (>= x 5) (< x 3))")
  end
end

