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
          puts "DEPTH: #{depth}"
          mark!(depth) if depth < @depth
          @names[depth] = [] if depth >= @depth
          @depth = depth
        end
        
        def put(name, expression)
          puts "PUT: #{name} : #{expression}"
          @names[@depth] << name.to_s
          @data[name.to_s] ||= Splice.new(@depth)
          @data[name.to_s] << expression unless expression.nil?
        end
        
        def inspecting(depth)
          puts "INSPECT: #{depth}"
          @inspecting = true
          self.depth = depth
        end
        
        def get(name)
          puts "GET: #{name}"
          @inspecting ? @names[@depth] << name.to_s :
                        @data[name.to_s].read
        end
        
        def defined?(name)
          @data.has_key?(name.to_s)
        end
        
        def expand!
          puts "EXPAND (#{@depth})"
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
          # TODO complain if sets are mismatched
          names = @names[@depth].uniq
          sizes = @data.select { |k,v| names.include?(k.to_s) }.
                        map { |pair| pair.last.size(@depth) }
          puts "SIZE: #{@depth} : #{names * ', '} -> #{sizes.inspect}"
          sizes.uniq.first
        end
        
        def iterate!
          puts "ITERATE!"
          @data.each do |name, splice|
            splice.shift!(@depth) if @names[@depth].include?(name)
          end
        end
      end
      
    end
  end
end

