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
          @names[depth] = []
          @depth = depth
        end
        
        def put(name, expression)
          puts "PUT: #{name} : #{expression}"
          @data[@depth] ||= {}
          @names[@depth] << name.to_s
          scope = @data[@depth]
          scope[name.to_s] ||= Splice.new
          scope[name.to_s] << expression unless expression.nil?
        end
        
        def inspecting(depth)
          puts "INSPECT: #{depth}"
          @inspecting = true
          self.depth = depth
        end
        
        def get(name)
          puts "GET: #{name}"
          @inspecting ? @names[@depth] << name.to_s :
                        @data[@depth][name.to_s].read
        end
        
        def defined?(name)
          data = @data[@depth]
          data && data.has_key?(name.to_s)
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
            data = @data[d]
            @names[d].uniq.each { |name| data[name].mark!(depth) }
            d += 1
          end
        end
        
        def size
          # TODO complain if sets are mismatched
          names = @names[@depth].uniq
          puts "SIZE: #{@depth} : #{names * ', '}"
          @data[@depth].select { |k,v| names.include?(k.to_s) }.
                        map { |pair| pair.last.size }.uniq.first
        end
        
        def iterate!
          puts "ITERATE!"
          @data[@depth].each do |name, splice|
            splice.shift! if @names[@depth].include?(name)
          end
        end
      end
      
    end
  end
end

