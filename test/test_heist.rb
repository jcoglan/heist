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
    Heist.info(@@env)
    
    @@env.define('assert') do |value|
      assert(value)
    end
    @@env.define('assert-equal') do |expected, actual|
      assert_equal(expected, actual)
    end
    @@env.syntax('assert-raise') do |scope, cells|
      exception = Heist.const_get(cells.car.to_s)
      assert_raise(exception) { scope.eval(cells.cdr.car) }
    end
  end
  
  %w[   booleans
        numbers
        lists
        arithmetic
        define_values
        define_functions
        closures
        let
        conditionals
        file_loading
        macros
        delay
        
  ].each do |test|
    define_method('test_' + test) do
      @@env.run($dir + '/' + test)
    end
  end
  
  def test_cons
    cons = Heist::Runtime::Cons
    
    empty = cons[[]]
    assert empty.list?
    assert !empty.pair?
    assert_equal cons::NULL, empty
    assert_equal 0, empty.length
    
    single = cons[[4]]
    assert single.list?
    assert single.pair?
    assert_equal 1, single.length
    
    multi = cons[[2,4,7]]
    assert multi.list?
    assert multi.pair?
    assert_equal 3, multi.length
    
    multi.tail.cdr = 8
    assert multi.pair?
    assert !multi.list?
    assert_raise(Heist::TypeError) { multi.size }
    
    nested = cons[[2,4,6,cons.new(7,8)]]
    assert nested.list?
    assert nested.pair?
    assert_equal 4, nested.length
    assert !nested.cdr.cdr.cdr.car.list?
    assert nested.cdr.cdr.cdr.car.pair?
    assert_equal 8, nested.cdr.cdr.cdr.car.cdr
  end
  
  def test_macro_hygiene
    @@env.run($dir + '/' + (@@env.hygienic? ? 'hygienic' : 'unhygienic'))
  end
  
  def test_continuations
    return if @@env.stackless?
    @@env.run($dir + '/continuations')
  end
  
  def test_quotes
    assert_equal 7, @@env.eval("(+ 3 4)")
    assert_equal [:+, 3, 4], @@env.eval("'(+ 3 4)").to_a
    assert Heist::Runtime::Cons === @@env.eval("'(+ 3 4)")
    assert_equal 7, @@env.eval("(+ '3 4)")
    assert_equal [:+, [:-, 7, 9], 4], @@env.eval("'(+ (- 7 9) 4)").to_a
    assert_equal [7, 9, 6], @@env.eval("`(7 ,(+ 4 5) 6)").to_a
    assert Heist::Runtime::Cons === @@env.eval("`(7 ,(+ 4 5) 6)")
    assert_equal [3, 7, 6, 2, 6, 9], @@env.eval("`(3 7 6 ,@((lambda () '(2 6))) 9)").to_a
    assert_equal [:quote, []], @@env.eval("''()").to_a
  end
  
  def test_birds
    return unless @@env.lazy?
    @@env.eval('(load "birdhouse")')
    
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
  
  def test_ruby_execution
    expr = [[:lambda, [:x], [:+, 1, :x]], 3]
    assert_equal 4, @@env.exec(expr)
    list = Heist.parse(expr)
    assert Heist::Runtime::Cons === list
    assert_equal expr, list.to_ruby
  end
end

