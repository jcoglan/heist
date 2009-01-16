# if -- cannot be written in Scheme using applicative order
metadef('if') do |frame, scope, cond, cons, alt|
  which = cond.eval(scope) ? cons : alt
  frame.push(which, scope)
end

