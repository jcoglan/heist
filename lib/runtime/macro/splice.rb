module Heist
  class Runtime
    class Macro
      
      class Splice
        attr_reader :depth
        
        def initialize(depth)
          @depth = depth
          @data  = []
          (0...@depth).inject(@data) { |list, d| list << []; list.last }
          @index = 0
          @stack = []
        end
        
        def <<(value)
          @stack.pop.call() while not @stack.empty?
          tail(@depth) << value
          puts @data.inspect
        end
        
        def mark!(depth)
          puts "MARK : #{depth}"
          @stack << lambda { tail(depth) << [] }
        end
        
        def size(depth = nil)
          data.size
        end
        
        def read
          value = data[@index]
          value
        end
        
        def shift!
          @index += 1
          @index = 0 if @index >= size
        end
        
      private
        
        def tail(depth)
          (0...depth).inject(@data) { |list, d| list.last }
        end
        
        def data
          @depth == 0 ? @data : @data.first
        end
      end
      
    end
  end
end

