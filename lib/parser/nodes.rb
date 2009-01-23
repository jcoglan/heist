module Heist
  module Scheme
    
    class Program < Treetop::Runtime::SyntaxNode
      def eval(scope)
        convert!
        @data.map { |part| Heist.value_of(part, scope) }.last
      end
      
      def convert!
        @data ||= elements.map { |e| e.eval }
      end
    end
    
    module List
      def eval
        Runtime::List.new(cells.map { |c| c.eval })
      end
      
      def cells
        @cells ||= elements[1].elements
      end
    end
    
    class Cell < Treetop::Runtime::SyntaxNode
      def eval
        result = elements[2].eval
        elements[1].text_value == "'" ?
            Runtime::List.new([Runtime::Identifier.new(:quote), result]) :
            result
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

