module Heist
  class Runtime
    class Macro
      
      class Matches
        def initialize(names)
          @data = {}
          names.each { |name| @data[name] = Splice.new(name) }
        end
        
        def descend!(names, depth)
          @data.each do |name, set|
            set.descend!(depth) if names.include?(name)
          end
        end
        
        def put(name, value)
          name = name.to_s
          @data[name] << value if has?(name)
        end
        
        def has?(name)
          @data.has_key?(name.to_s)
        end
        
        def get(name)
          @data[name.to_s].read
        end
        
        def expand!(repeater, depth)
          names = Macro.pattern_vars(repeater)
          size(names, depth).times { yield and iterate!(names, depth) }
        end
        
      private
        
        def size(names, depth)
          sizes = []
          @data.each do |name, set|
            sizes << set.size(depth) if names.include?(name)
          end
          
          sizes.uniq!
          return sizes.first if sizes.size == 1
          
          raise MacroTemplateMismatch.new(
            "Macro could not be expanded: mismatched repetition patterns")
        end
        
        def iterate!(names, depth)
          @data.each do |name, set|
            set.shift!(depth) if names.include?(name)
          end
        end
      end
      
    end
  end
end

