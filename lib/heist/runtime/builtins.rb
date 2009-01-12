module Heist
  module Runtime
    module Builtins
      def self.add(env)
        
        env["define"] = MetaFunction.new(env) do |scope, names, body|
          names = names.as_string
          env[names.first] = Function.new(scope, names[1..-1], body)
        end
        
        env["lambda"] = MetaFunction.new(env) do |scope, names, body|
          Function.new(scope, names.as_string, body)
        end
        
        env["display"] = Function.new(env) do |expression|
          puts expression
        end
        
        env["+"] = Function.new(env) do |op1, op2|
          String === op1 || String === op2 ?
              op1.to_s + op2.to_s :
              op1 + op2
        end
        
        env["-"] = Function.new(env) do |op1, op2|
          op1 - op2
        end
        
        env["*"] = Function.new(env) do |op1, op2|
          op1 * op2
        end
        
        env["/"] = Function.new(env) do |op1, op2|
          op1 / op2.to_f
        end
          
      end
    end
  end
end

