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
        scope.runtime.eval_list(self, scope)
      end
      
      def cells
        @cells ||= elements[1].elements.map { |e| e.data }
      end
      
      def function(scope)
        cells.first.eval(scope)
      end
      
      def arguments
        cells[1..-1]
      end
      
      def as_string
        cells.map { |c| c.as_string }
      end
    end
    
    class Cell < Treetop::Runtime::SyntaxNode
      def eval(scope)
        data.eval(scope)
      end
      
      def data
        elements[1]
      end
      
      def as_string
        data.as_string
      end
    end
    
    module Atom
      def as_string
        text_value
      end
    end
    
    module Boolean
      def eval(scope)
        @value ||= (text_value == "#t")
      end
    end
    
    class Complex < Treetop::Runtime::SyntaxNode
      def eval(scope)
        # TODO
      end
    end
    
    class Real < Treetop::Runtime::SyntaxNode
      def eval(scope)
        @value ||= Kernel.eval(text_value).to_f
      end
    end
    
    class Rational < Treetop::Runtime::SyntaxNode
      def eval(scope)
        @value ||= numerator.eval(scope).to_f / denominator.eval(scope)
      end
    end
    
    class Integer < Treetop::Runtime::SyntaxNode
      def eval(scope)
        @value ||= Kernel.eval(text_value).to_i
      end
    end
    
    class String < Treetop::Runtime::SyntaxNode
      def eval(scope)
        @value ||= Kernel.eval(text_value)
      end
    end
    
    class Identifier < Treetop::Runtime::SyntaxNode
      def eval(scope)
        scope[text_value]
      end
    end
    
  end
end

