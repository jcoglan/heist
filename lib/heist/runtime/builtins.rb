module Heist
  class Runtime
    module Builtins
      def self.add(env)
        
        env["define"] = MetaFunction.new(env) do |scope, names, body|
          names = names.as_string
          scope[names.first] = (Array === names) ?
              Function.new(scope, names[1..-1], body) :
              body.eval(scope)
        end
        
        env["lambda"] = MetaFunction.new(env) do |scope, names, body|
          Function.new(scope, names.as_string, body)
        end
        
        env["display"] = Function.new(env) do |expression|
          puts expression.eval
        end
        
        env["+"] = Function.new(env) do |*args|
          args = args.map { |arg| arg.eval }
          args.any? { |arg| String === arg } ?
              args.inject("") { |str, arg| str + arg.to_s } :
              args.inject(0)  { |sum, arg| sum + arg }
        end
        
        env["-"] = Function.new(env) do |op1, op2|
          op2.nil? ? 0 - op1.eval : op1.eval - op2.eval
        end
        
        env["*"] = Function.new(env) do |*args|
          args.inject(1) { |prod, arg| prod * arg.eval }
        end
        
        env["/"] = Function.new(env) do |op1, op2|
          op1.eval / op2.eval.to_f
        end
        
        env["cond"] = MetaFunction.new(env) do |scope, *pairs|
          matched, result = false, nil
          pairs.each do |pair|
            next if matched
            matched = pair.cells.first.eval(scope)
            
            if matched ||
                (pair == pairs.last && pair.cells.first.as_string == "else")
              result  = pair.cells.last.eval(scope)
            end
          end
          result
        end
        
        env["="] = Function.new(env) do |op1, op2|
          op1.eval == op2.eval
        end
        
        env[">"] = Function.new(env) do |op1, op2|
          op1.eval > op2.eval
        end
        
        env["<"] = Function.new(env) do |op1, op2|
          op1.eval < op2.eval
        end
        
        env["and"] = MetaFunction.new(env) do |scope, *args|
          result = true
          args.each do |arg|
            next if !result
            result = arg.eval(scope)
          end
          result
        end
        
        env["or"] = MetaFunction.new(env) do |scope, *args|
          result = false
          args.each do |arg|
            next if result
            result = arg.eval(scope)
          end
          result
        end
        
        env["not"] = Function.new(env) do |expr|
          !expr.eval
        end
        
        Heist.run(PATH + 'builtins.scm', env)
          
      end
    end
  end
end

