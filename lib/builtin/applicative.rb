# if -- cannot be written in Scheme using applicative order
self["if"] = MetaFunction.new(self) do |frame, scope, cond, cons, alt|
  which = cond.eval(scope) ? cons : alt
  if Scheme::List === which
    function, bindings = *which.bindings(scope)
    frame.push(function, scope, bindings)
  else
    frame.send(which.eval(scope))
  end
end

