module Heist
  module Scheme
    
    SHORTHANDS = {
      "'"   => :quote,
      "`"   => :quasiquote,
      ","   => :unquote,
      ",@"  => :'unquote-splicing'
    }
    
    class Program < Treetop::Runtime::SyntaxNode
      def eval(scope)
        convert!
        @data.map { |part| Heist.evaluate(part, scope) }.last
      end
      
      def convert!
        return if @data
        @data = Runtime::Cons.construct(elements, true) { |c| c.eval }
      end
    end
    
    module List
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
    
    class QuotedCell < Treetop::Runtime::SyntaxNode
      def eval
        quote = elements[1].text_value
        cell  = elements[2].eval
        Runtime::Cons.construct([Runtime::Identifier.new(SHORTHANDS[quote]), cell])
      end
    end
    
    class Cell < Treetop::Runtime::SyntaxNode
      def eval
        elements[1].eval
      end
    end
    
    class Datum < Treetop::Runtime::SyntaxNode
      def eval
        elements[0].eval
      end
    end
    
    module Boolean
      def eval
        @value ||= (text_value == "#t")
      end
    end
    
    class Complex < Treetop::Runtime::SyntaxNode
      def eval
        # TODO
      end
    end
    
    class Real < Treetop::Runtime::SyntaxNode
      def eval
        @value ||= Kernel.eval(text_value).to_f
      end
    end
    
    class Rational < Treetop::Runtime::SyntaxNode
      def eval
        @value ||= numerator.eval.to_f / denominator.eval
      end
    end
    
    class Integer < Treetop::Runtime::SyntaxNode
      def eval
        @value ||= Kernel.eval(text_value).to_i
      end
    end
    
    class String < Treetop::Runtime::SyntaxNode
      def eval
        @value ||= Kernel.eval(text_value)
      end
    end
    
    module Identifier
      def eval
        Runtime::Identifier.new(text_value)
      end
    end
    
  end
end

