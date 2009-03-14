module Heist
  class Runtime
    
    class Scope
      attr_reader :runtime
      
      def initialize(parent = {})
        @symbols = {}
        is_runtime = (Runtime === parent)
        @parent = is_runtime ? {} : parent
        @runtime = is_runtime ? parent : parent.runtime
      end
      
      def [](name)
        name = to_name(name)
        bound = @symbols.has_key?(name)
        
        raise UndefinedVariable.new(
          "Variable '#{name}' is not defined") unless bound or Scope === @parent
            
        value = bound ? @symbols[name] : @parent[name]
        value = value.extract! if value.respond_to?(:extract!)
        value
      end
      
      def []=(name, value)
        @symbols[to_name(name)] = value
        value.name = name if Function === value
        value
      end
      
      def defined?(name)
        @symbols.has_key?(to_name(name)) or
            (Scope === @parent and @parent.defined?(name))
      end
      
      def innermost_binding(name)
        name = to_name(name)
        @symbols.has_key?(name) ?
            self :
        Scope === @parent ?
            @parent.innermost_binding(name) :
            nil
      end
      
      def set!(name, value)
        name = to_name(name)
        bound = @symbols.has_key?(name)
        
        raise UndefinedVariable.new(
          "Cannot set undefined variable '#{name}'") unless bound or Scope === @parent
        
        return @parent.set!(name, value) unless bound
        self[name] = value
      end
      
      def define(name, *args, &block)
        self[name] = Function.new(self, *args, &block)
      end
      
      def syntax(name, holes = [], &block)
        self[name] = Syntax.new(self, holes,&block)
      end
      
      def grep(pattern)
        base = (Scope === @parent) ? @parent.grep(pattern) : []
        @symbols.each do |key, value|
          base << key if key =~ pattern
        end
        base.uniq
      end
      
      # TODO: this isn't great, figure out a way for functions
      # to transparently handle inter-primitive calls so Ruby can
      # call Scheme code as well as other Ruby code
      def call(name, *params)
        self[name].body.call(*params)
      end
      
      # Note that local vars in this method can cause block vars
      # to become delocalized when running Ruby files under 1.8,
      # so make sure we use 'obscure' names here.
      def run(_path)
        return instance_eval(File.read(_path)) if File.extname(_path) == '.rb'
        _path   = _path + FILE_EXT unless File.file?(_path)
        _source = Heist.parse(File.read(_path))
        _scope  = FileScope.new(self, _path)
        _source.eval(_scope)
      end
      
      def eval(source)
        source = Heist.parse(source)
        source.eval(self)
      end
      alias :exec :eval
      
      def load(path)
        dir = load_path.find do |dir|
          File.file?("#{dir}/#{path}") or File.file?("#{dir}/#{path}#{FILE_EXT}")
        end
        return false unless dir
        runtime.run("#{dir}/#{path}")
        true
      end
      
      def current_file
        @path || @parent.current_file rescue nil
      end
      
    private
      
      def to_name(name)
        name.to_s.downcase
      end
      
      def keyword?(scope, expression, name)
        expression == Identifier.new(name) &&
            innermost_binding(name) == scope.innermost_binding(expression)
      end
      
      def load_path
        paths, file = [], current_file
        paths << File.dirname(file) if file
        paths + LOAD_PATH
      end
    end
    
    class FileScope < Scope
      extend Forwardable
      def_delegators(:@parent, :[]=)
      
      def initialize(parent, path)
        super(parent)
        @path = File.directory?(path) ? path + '/repl.scm' : path
      end
    end
    
  end
end

