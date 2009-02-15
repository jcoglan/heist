module Heist
  class Runtime
    class Macro
      
      class Matches
        def initialize
          @data  = {}
          @depth = 0
          @names = []
        end
        
        def depth=(depth)
          mark!(depth) if depth < @depth
          @names[depth] = [] if depth >= @depth
          @depth = depth
        end
        
        def put(name, expression)
          name = name.to_s
          @names[@depth] << name
          @data[name] ||= Splice.new(name, @depth)
          @data[name] << expression unless expression.nil?
        end
        
        def inspecting(depth)
          @inspecting = true
          self.depth = depth
        end
        
        def get(name)
          @inspecting ? @names[@depth] << name.to_s :
                        @data[name.to_s].read
        end
        
        def defined?(name)
          @data.has_key?(name.to_s)
        end
        
        def expand!
          @inspecting = false
          size.times { yield and iterate! }
        end
        
      private
        
        def mark!(depth)
          d = @depth
          while @names[d]
            @names[d].uniq.each { |name| @data[name].mark!(depth) }
            d += 1
          end
        end
        
        def size
          names   = @names[@depth].uniq
          splices = @data.select { |k,v| names.include?(k.to_s) }
          sizes   = splices.map { |pair| pair.last.size(@depth) }.uniq
          
          return 0 if sizes.empty?
          return sizes.first if sizes.size == 1
          
          expressions = splices.map { |pair| '"' + pair.last.to_s(@depth) + '"' } * ', '
          raise MacroTemplateMismatch.new(
            "Macro could not be expanded: expressions #{expressions} are of different sizes")
        end
        
        def iterate!
          @data.each do |name, splice|
            splice.shift!(@depth) if @names[@depth].include?(name)
          end
        end
      end
      
    end
  end
end

