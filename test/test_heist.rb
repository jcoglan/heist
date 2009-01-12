require File.dirname(__FILE__) + "/../lib/heist"
require "test/unit"

class HeistTest < Test::Unit::TestCase
  def new_scope!
    @scope = Heist::Runtime::Scope.new
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
  
  def test_define
    new_scope!
    @scope.eval("(define size 2)")
    assert_equal 2, @scope.eval("size")
    
    @scope.eval <<-CODE
      (define pi 3.14159)
      (define radius 10)
    CODE
    assert_equal 314.159, @scope.eval("(* pi (* radius radius))")
    
    @scope.eval("(define circumference (* 2 pi radius))")
    assert_equal 62.8318, @scope.eval("circumference")
  end
end

