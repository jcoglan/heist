require 'forwardable'

module Heist
  class Runtime
    
    class Binding
      attr_reader :expression, :refs
      
      extend Forwardable
      def_delegators(:@scope, :[]=)
      
      def self.create(expression, scope)
        return new(expression, scope) unless Scheme::Identifier === expression
        value = expression.eval(scope)
        Binding === value ? value : new(expression, scope)
      end
      
      def initialize(expression, scope)
        @expression, @scope = expression, scope
        @deps, @refs = [], []
        store_references! if scope.runtime.lazy_inversion?
      end
      
      def eval(scope = nil)
        return @value if defined?(@value)
        find_values! unless has_all_values?
        @value = @expression.eval(@scope)
        @deps.each { |dep| dep.first[dep.last] = @value }
        @value
      end
      
      def has_dependency(binding, name)
        @deps << [binding, name]
      end
      
    private
      
      def store_references!
        return unless Scheme::List === @expression
        @expression.cells.each do |cell|
          next unless Scheme::Identifier === cell
          value = cell.eval(@scope)
          next unless Binding === value
          @refs << value
          value.has_dependency(self, cell.text_value)
        end
      end
      
      def find_values!
        @list = [self]
        expand! while !@ready
        @list.each { |ref| ref.eval }
      end
      
      def has_all_values?
        return true unless Scheme::List === @expression
        @expression.cells.all? do |cell|
          !(Scheme::Identifier === cell) || !(Binding === cell.eval(@scope))
        end
      end
      
      def expand!
        current_size = @list.size
        @list = @list.map { |b| b.refs + [b] }.flatten.uniq - [self]
        @ready = true if @list.size == current_size
      end
    end
    
  end
end

