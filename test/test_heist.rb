require File.dirname(__FILE__) + "/../lib/heist"
require "test/unit"

class HeistTest < Test::Unit::TestCase
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
end

