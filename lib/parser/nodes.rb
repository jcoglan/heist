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
        @data = Runtime::Cons.construct(elements) { |c| c.eval }
      end
    end
    
    module List
      def eval
        Runtime::Cons.construct(cells, true) { |c| c.eval }
      end
      
      def cells
        @cells ||= elements[1].elements
      end
    end
    
    class Cell < Treetop::Runtime::SyntaxNode
      def eval
        result = elements[3].eval
        string = elements[1].text_value
        SHORTHANDS.has_key?(string) ?
            Runtime::Cons.construct([Runtime::Identifier.new(SHORTHANDS[string]), result]) :
            result
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
    
    class Identifier < Treetop::Runtime::SyntaxNode
      def eval
        Runtime::Identifier.new(text_value)
      end
    end
    
  end
end

