module Heist
  class Runtime
    class Macro
      
      class Splice
        attr_reader :name, :depth
        
        def initialize(name, depth)
          @name, @depth = name, depth
          @data  = []
          (0...@depth).inject(@data) { |list, d| list << []; list.last }
          @indexes = (0..@depth).map { 0 }
          @stack = []
        end
        
        def <<(value)
          return if Cons::NULL == value
          @stack.pop.call() while not @stack.empty?
          tail(@depth) << value
        end
        
        def mark!(depth)
          @stack << lambda { tail(depth) << [] }
        end
        
        def size(depth)
          current = current(depth)
          empty?(current) ? 0 : current.size
        end
        
        def read
          current(@depth)[@indexes[@depth]]
        end
        
        def shift!(depth)
          @indexes[depth] += 1
          @indexes[depth] = 0 if @indexes[depth] >= current(depth).size
        end
        
        def to_s(depth = 0)
          "#{@name}#{' ...' * (@depth - depth)}"
        end
        
      private
        
        def tail(depth)
          (0...depth).inject(@data) { |list, d| list.last }
        end
        
        def current(depth)
          @indexes[0...depth].inject(@data) { |list, i| list[i] }
        end
        
        def empty?(array)
          array == [] or array == [[]]
        end
      end
      
    end
  end
end

