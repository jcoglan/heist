module Heist
  # The +Scheme+ module hosts various classes used by the +SchemeParser+ class,
  # which is generated from a parsing expression grammar using +Treetop+. (See
  # <tt>lib/parser/scheme.tt</tt>.) The classes map syntax structures generated
  # by +Treetop+ to Heist runtime objects for execution. All the classes except
  # +Program+ are evaluated without a runtime environment; evaluating them
  # simply casts to non-Treetop objects in the Heist library, or to raw Ruby
  # objects. Evaluating a +Program+ requires a +Runtime+ in which to do so.
  module Scheme
    
    # Any list-generating shorthands present in the grammar should be listed
    # here. In Scheme, this list includes the various quoting symbols that can
    # be used as shorthands for calling quoting functions.
    SHORTHANDS = {
      "'"   => 'quote',
      "`"   => 'quasiquote',
      ","   => 'unquote',
      ",@"  => 'unquote-splicing'
    }
    
    # +Program+ is the root of the parse tree; parsing any string of Scheme code
    # produces one of these.
    class Program < Treetop::Runtime::SyntaxNode
      # Evaluates all the expressions in the +Program+ in order, returning the
      # result of the last expression.
      def eval(scope)
        convert!
        @data.map { |part| Heist.evaluate(part, scope) }.last
      end
      
      # Converts all the +Treetop+ objects in the +Program+ to Heist objects and
      # raw Ruby data ready for interpretation using a +Runtime+.
      def convert!
        return @data if @data
        @data = Runtime::Cons.construct(elements[1].elements, true) { |c| c.eval }
      end
    end
    
    # A +List+ has an array of +cells+, and optionally a +tail+ if it's an
    # improper list or a dotted pair.
    module List
      # Evaluating a +List+ produces a Heist +Cons+ object.
      def eval
        list = Runtime::Cons.construct(cells, true) { |c| c.eval }
        list.tail.cdr = tail.cell.eval if tail.respond_to?(:dot)
        list
      end
      
      def cells
        @cells ||= elements[1].elements[0].elements
      end
      
      def tail
        @tail ||= elements[1].elements[1]
      end
    end
    
    # A +Vector+ is an array-like structure if integer-indexed cells
    module Vector
      def eval
        Runtime::Vector.new(cells) { |cell| cell.eval }
      end
      
      def cells
        @cells ||= elements[2].elements
      end
    end
    
    # <tt>QuotedCell</tt> are generated using the quoting shorthands.
    class QuotedCell < Treetop::Runtime::SyntaxNode
      # Evaluating a +QuotedCell+ produces a +Cons+ that expresses a function
      # call to the appropriate quoting function, with the cell as the argument.
      def eval
        quote = elements[1].text_value
        cell  = elements[2].eval
        Runtime::Cons.construct([Runtime::Identifier.new(SHORTHANDS[quote]), cell])
      end
    end
    
    # <tt>Cells</tt> are any piece of Scheme data: numbers, booleans, strings,
    # lists. Any building block of Scheme code goes in a +Cell+.
    class Cell < Treetop::Runtime::SyntaxNode
      def eval
        elements[1].eval
      end
    end
    
    # A +Datum+ is any piece of atomic literal data.
    class Datum < Treetop::Runtime::SyntaxNode
      def eval
        elements[0].eval
      end
    end
    
    class Boolean < Treetop::Runtime::SyntaxNode
      def eval
        @value ||= (text_value == "#t")
      end
    end
    
    class Complex < Treetop::Runtime::SyntaxNode
      def eval
        @value ||= Complex(real.eval, imaginary.eval)
      end
    end
    
    class Real < Treetop::Runtime::SyntaxNode
      def eval
        @value ||= text_value.to_f
      end
    end
    
    class Rational < Treetop::Runtime::SyntaxNode
      def eval
        @value ||= Rational(numerator.eval, denominator.eval)
      end
    end
    
    class Integer < Treetop::Runtime::SyntaxNode
      def eval
        @value ||= text_value.to_i
      end
    end
    
    class Character < Treetop::Runtime::SyntaxNode
      def eval
        Runtime::Character.new(glyph.text_value)
      end
    end
    
    class String < Treetop::Runtime::SyntaxNode
      def eval
        return @value if @value
        @value = Kernel.eval(text_value)
        @value.freeze
        @value
      end
    end
    
    module Identifier
      def eval
        Runtime::Identifier.new(text_value)
      end
    end
    
  end
end

