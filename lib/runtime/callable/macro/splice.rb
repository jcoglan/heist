module Heist
  class Runtime
    class Macro
      
      class Splice
        def initialize
          @data  = []
          @depth = 0
          @queue = []
        end
        
        def descend!(depth)
          @queue.shift.call() while not @queue.empty?
          @queue << lambda { tail(depth-1) << [] }
          @depth = depth if depth > @depth
        end
        
        def <<(value)
          return if Cons::NULL == value
          @queue.shift.call() while not @queue.empty?
          tail(@depth) << value
        end
        
        def read
          current(@depth)[indexes[@depth]]
        end 
               
        def shift!(depth)
          indexes[depth] += 1
          indexes[depth] = 0 if indexes[depth] >= current(depth).size
        end
        
        def size(depth)
          size = current(depth).size rescue 0
          size
        end
        
      private
        
        def tail(depth)
          (0...depth).inject(@data) { |list, d| list.last }
        end
        
        def current(depth)
          indexes[0...depth].inject(@data) { |list, i| list[i] }
        end
        
        def indexes
          @indexes ||= (0..@depth).map { 0 }
          @indexes
        end
      end
      
    end
  end
end

