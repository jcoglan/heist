$VERBOSE = nil
$dir = File.dirname(__FILE__)

require $dir + "/../lib/heist"
require "test/unit"

Class.new(Test::Unit::TestCase) do
  def setup
    return @@env if defined?(@@env)
    @@env = Heist::Runtime.new
    puts "\nType of if() function: #{ @@env["if"].class }"
    puts "Application mode: #{ Heist::ORDERS[@@env.order] }\n\n"
    
    @@env.define('assert') do |value|
      assert(value)
    end
    @@env.define('assert-equal') do |expected, actual|
      assert_equal(expected, actual)
    end
  end
  
  %w[   booleans
        numbers
        arithmetic
        define_values
        define_functions
        closures
        conditionals
        
  ].each do |test|
    define_method('test_' + test) do
      Heist.run($dir + '/' + test, @@env)
    end
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

