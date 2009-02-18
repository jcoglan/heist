$VERBOSE = nil
$dir = File.dirname(__FILE__)

$args = ARGV.map { |opt| "-#{opt}" }

require $dir + "/../lib/heist"
require $dir + "/../lib/bin_spec"
require "test/unit"

Class.new(Test::Unit::TestCase) do
  @@env = nil
  
  def setup
    return @@env if @@env
    @@env = Heist::Runtime.new(Heist::BIN_SPEC.parse($args))
    puts "\nType of if() function: #{ @@env["if"].class }"
    puts "Application mode: #{ Heist::ORDERS[@@env.order] }\n\n"
    
    @@env.define('assert') do |value|
      assert(value)
    end
    @@env.define('assert-equal') do |expected, actual|
      assert_equal(expected, actual)
    end
    @@env.syntax('assert-raise') do |scope, name, expression|
      exception = Heist.const_get(name.to_s)
      assert_raise(exception) { @@env.eval(expression) }
    end
  end
  
  %w[   booleans
        numbers
        arithmetic
        define_values
        define_functions
        closures
        let
        conditionals
        file_loading
        macros
        
  ].each do |test|
    define_method('test_' + test) do
      @@env.run($dir + '/' + test)
    end
  end
  
  def test_continuations
    return if @@env.stackless?
    @@env.run($dir + '/continuations')
  end
  
  def test_quotes
    assert_equal 7, @@env.eval("(+ 3 4)")
    assert_equal [:+, 3, 4], @@env.eval("'(+ 3 4)").to_a
    assert Heist::Runtime::List === @@env.eval("'(+ 3 4)")
    assert_equal 7, @@env.eval("(+ '3 4)")
    assert_equal [:+, [:-, 7, 9], 4], @@env.eval("'(+ (- 7 9) 4)").to_a
    assert_equal [7, 9, 6], @@env.eval("`(7 ,(+ 4 5) 6)").to_a
    assert Heist::Runtime::List === @@env.eval("`(7 ,(+ 4 5) 6)")
    assert_equal [3, 7, 6, 2, 6, 9], @@env.eval("`(3 7 6 ,@((lambda () '(2 6))) 9)").to_a
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

