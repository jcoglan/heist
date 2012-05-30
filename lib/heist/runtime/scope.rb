module Heist
  class Runtime
    
    # +Scope+ is primarily used to represent symbol tables, though it also
    # has a few other scope-related responsibilities such as defining
    # functions (functions need to remember the scope they appear in) and
    # loading files. Scheme uses lexical scope, which we model using a simple
    # delegation system.
    #
    # Every +Scope+ has a hash (<tt>@symbols</tt>) in which it stores names
    # of variables and their associated values, and a parent scope
    # (<tt>@parent</tt>). If a variable cannot be found in one scope, the
    # lookup is delegated to the parent until we get to the top level, at
    # which point an exception is raised.
    #
    class Scope
      attr_reader :runtime
      
      # A +Scope+ is initialized using another +Scope+ to use as the parent.
      # The parent may also be a +Runtime+ instance, indicating that the
      # new +Scope+ is being used as the top level of a runtime environment.
      def initialize(parent = {})
        @symbols = Trie.new
        is_runtime = (Runtime === parent)
        @parent = is_runtime ? {} : parent
        @runtime = is_runtime ? parent : parent.runtime
      end
      
      # Returns the value corresponding to the given variable name. If the
      # name does not exist in the receiver, the call is delegated to its
      # parent scope. If the name cannot be found in any scope an exception
      # is raised.
      #
      # In lazy mode, +Binding+ objects are stored in the symbol table when
      # functions are called; we do not evaluate the arguments to a function
      # before calling it, but instead we force an argument's value if the
      # function's body attempts to access it by name.
      #
      def [](name)
        name = to_name(name)
        return Keyword.new(name[0..-2]) if name.end_with?(':')
        bound = @symbols.has_key?(name)
        
        raise UndefinedVariable.new(
          "Variable '#{name}' is not defined") unless bound or Scope === @parent
            
        value = bound ? @symbols[name] : @parent[name]
        value = value.force! if value.respond_to?(:force!)
        value
      end
      
      # Binds the given +value+ to the given +name+ in the receiving +Scope+.
      # Note this always sets the variable in the receiver; see <tt>set!</tt>
      # for a method corresponding to Scheme's <tt>(set!)</tt> function.
      def []=(name, value)
        @symbols[to_name(name)] = value
        value.name = name if Function === value
        value
      end
      
      # Returns +true+ iff the given name is bound as a variable in the
      # receiving scope or in any of its ancestor scopes.
      def defined?(name)
        @symbols.has_key?(to_name(name)) or
            (Scope === @parent and @parent.defined?(name))
      end
      
      # Returns a +Scope+ object representing the innermost scope in which
      # the given name is bound. This is used to find out whether two or
      # more identifiers have the same binding.
      #
      #   outer = Scope.new
      #   outer['foo'] = "a value"
      #
      #   inner = Scope.new(outer)
      #   inner['bar'] = "something"
      #
      #   inner.innermost_binding('foo') #=> outer
      #   inner.innermost_binding('bar') #=> inner
      #
      def innermost_binding(name)
        name = to_name(name)
        @symbols.has_key?(name) ?
            self :
        Scope === @parent ?
            @parent.innermost_binding(name) :
            nil
      end
      
      # Analogous to Scheme's <tt>(set!)</tt> procedure. Assigns the given
      # +value+ to the given variable +name+ in the innermost region in
      # which +name+ is bound. If the +name+ does not exist in the receiving
      # scope, the assignment is delegated to the parent. If no visible
      # binding exists for the given +name+ an exception is raised.
      def set!(name, value)
        scope = innermost_binding(name)
        raise UndefinedVariable.new("Cannot set undefined variable '#{name}'") if scope.nil?
        scope[name] = value
      end

      # +define+ is used to define functions using either Scheme or Ruby
      # code. Takes either a name and a Ruby block to represent the function,
      # or a name, a list of formal arguments and a list of body expressions.
      # The <tt>(define)</tt> primitive exposes this method to the Scheme
      # environment. This method allows easy extension using Ruby, for
      # example:
      #
      #   scope.define('+') |*args|
      #     args.inject { |a,b| a + b }
      #   end
      #
      # See +Function+ for more information.
      #
      def define(name, *args, &block)
        self[name] = Function.new(self, *args, &block)
      end
      
      # +syntax+ is similar to +define+, but is used for defining syntactic
      # forms. Heist's parser has no predefined syntax apart from generic
      # Lisp paren syntax and Scheme data literals. All special forms are
      # defined as special functions and stored in the symbol table, making
      # them first-class objects that can be easily aliased and overridden.
      #
      # This method takes a name and a Ruby block. The block will be called
      # with the calling +Scope+ object and a +Cons+ containing the section
      # of the parse tree representing the parameters the form has been called
      # with.
      #
      # It is not recommended that you write your own syntax using Ruby
      # since it requires too much knowledge of the plumbing for features
      # like tail calls and continuations. If you define new syntax using
      # Scheme macros you get correct behaviour of these features for free.
      #
      # See +Syntax+ for more information.
      #
      def syntax(name, &block)
        self[name] = Syntax.new(self, &block)
      end
      
      # Calls the given +block+ with the name of every variable visible in
      # the +Scope+ (given as a +Symbol+) and its corresponding value.
      def each_var(&block)
        @parent.each_var(&block) if @parent.respond_to?(:each_var)
        @symbols.each do |key, value|
          block.call((key * '').to_sym, value)
        end
      end

      # Parses and executes the given string of source code in the receiving
      # +Scope+. Accepts strings of Scheme source and arrays of Ruby data to
      # be interpreted as Scheme lists.
      def eval(source)
        source = Heist.parse(source)
        source.eval(self)
      end
      alias :exec :eval
      
      # Executes an array of Scheme statements expressed as Ruby data.
      def program(expressions)
        expressions.map { |expr| exec(expr) }.last
      end
      
      # Returns the longest shared prefix match for the given variable name
      # stub, used to support autocompletion in the REPL.
      def longest_prefix(name)
        @symbols.longest_prefix(to_name(name))
      end
      
      # Runs the given Scheme or Ruby definition file in the receiving
      # +Scope+. Note that local vars in this method can cause block vars
      # to become delocalized when running Ruby files under 1.8, so make
      # sure we use 'obscure' names here.
      def run(_path)
        return instance_eval(File.read(_path)) if File.extname(_path) == '.rb'
        _source = Heist.parse(File.read(_path))
        _scope  = FileScope.new(self, _path)
        _source.eval(_scope)
      end
      
      # Loads the given Scheme file and executes it in the global scope.
      # Paths are treated as relative to the current file. If no local file
      # is found, the path is assumed to refer to a module from the Heist
      # standard library. The <tt>(load)</tt> primitive is a wrapper
      # around this method.
      def load(file)
        path = expand_path(file)
        runtime.run(path) if path
      end
      
      # Returns the path of the current file. The receiving scope must have
      # a +FileScope+ as an ancestor, otherwise this method will return +nil+.
      def current_file
        @path || @parent.current_file rescue nil
      end
      
      # Returns an absolute path to a requested library based on searching
      # the current load path. The file extension may be omitted, suitable
      # extensions being listed in Heist::FILE_EXTS.
      def expand_path(path)
        load_path.each do |dir|
          test_path = File.expand_path(File.join(dir, path))
          FILE_EXTS.each do |ext|
            full_path = test_path + ext
            return full_path if File.file?(full_path)
          end
        end
        nil
      end
      
    private
      
      # Calls the named primitive function with the given arguments, and
      # returns the result of the call.
      #
      # TODO: this is currently hampered by the fact that Functions expect to
      # be called with a +Scope+, but Ruby primitives are not given the
      # current +scope+. Figure out something better.
      def call(name, *params)
        self[name].body.call(*params)
      end

      # Converts any Ruby object to a name string. All names are downcased
      # as this Scheme is case-insensitive.
      def to_name(name)
        name.to_s.downcase
      end
      
      # Returns the current set of directories in which to look for Scheme
      # files to load. Includes the standard library path by default, and
      # the directory of the current file if the receiving +Scope+ has a
      # +FileScope+ as an ancestor.
      def load_path
        paths, file = [""], current_file
        paths << File.dirname(file) if file
        paths + LOAD_PATH
      end
    end
    
    # A +FileScope+ is a special kind of +Scope+ used to represent the region
    # of a single file. It provides Scheme code with an awareness of its
    # path so it can load local files. +FileScope+ instances delegate all
    # variable assignments to their parent +Scope+ (this is typically the
    # global scope) so that variables are visible across files.
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

