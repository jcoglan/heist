# if -- cannot be written in Scheme using applicative order
self["if"] = MetaFunction.new(self) do |scope, cond, cons, alt|
  cond.eval(scope) ?
      cons.eval(scope) :
      alt.eval(scope)
end

