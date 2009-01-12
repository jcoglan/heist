module Heist
  module Scheme
    
    class Program < Treetop::Runtime::SyntaxNode
      def eval(scope)
        elements.map { |e| e.eval(scope) }.last
      end
    end
    
    class Comment < Treetop::Runtime::SyntaxNode
      def eval(scope)
        nil
      end
    end
    
    class List < Treetop::Runtime::SyntaxNode
      def eval(scope)
        cells.first.eval(scope).call(scope, *cells[1..-1])
      end
      
      def cells
        elements[2].elements
      end
      
      def as_string
        cells.map { |c| c.as_string }
      end
    end
    
    class Atom < Treetop::Runtime::SyntaxNode
      def eval(scope)
        elements[1].eval(scope)
      end
      
      def as_string
        elements[1].as_string
      end
    end
    
    module Symbol
      def eval(scope)
        scope[text_value]
      end
      
      def as_string
        text_value
      end
    end
    
    class Number < Treetop::Runtime::SyntaxNode
      def eval(scope)
        @value ||= Kernel.eval(text_value)
      end
      
      def as_string
        text_value
      end
    end
    
  end
end

