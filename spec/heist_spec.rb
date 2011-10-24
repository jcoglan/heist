require 'spec_helper'

describe Heist do
  let(:options) { {} }
  
  before do
    @runtime = Heist::Runtime.new(options)
    
    @runtime.define 'assert' do |value|
      value.should be_true
    end
    @runtime.define 'assert-equal' do |expected, actual|
      actual.should == expected
    end
    @runtime.syntax 'assert-raise' do |scope, cells|
      exception = Heist.const_get(cells.car.to_s)
      lambda { scope.eval(cells.cdr.car) }.should raise_error(exception)
    end
  end
  
  dir = File.expand_path('../scheme_tests', __FILE__)
  
  shared_examples_for "Scheme interpreter" do
    
    tests = Dir.entries(dir).grep(/\.scm$/) -
            %w[continuations.scm hygienic.scm unhygienic.scm]
    
    tests.each do |test|
      it("runs #{test}") { @runtime.run File.join(dir, test) }
    end
  end
  
  shared_examples_for "interpreter with continuations" do
    it "runs continuations.scm" do
      @runtime.run File.join(dir, "continuations.scm")
    end
  end
  
  shared_examples_for "interpreter with hygienic macros" do
    it "runs hygienic.scm" do
      @runtime.run File.join(dir, "hygienic.scm")
    end
  end
  
  shared_examples_for "interpreter with unhygienic macros" do
    it "runs unhygienic.scm" do
      @runtime.run File.join(dir, "unhygienic.scm")
    end
  end
  
  describe "with default options" do
    it_should_behave_like "Scheme interpreter"
    it_should_behave_like "interpreter with hygienic macros"
  end
  
  describe "with unhygienic macros" do
    let(:options) { {:unhygienic => true} }
    it_should_behave_like "Scheme interpreter"
    it_should_behave_like "interpreter with unhygienic macros"
    
    describe "with continuations" do
      let(:options) { {:unhygienic => true, :continuations => true} }
      it_should_behave_like "Scheme interpreter"
      it_should_behave_like "interpreter with unhygienic macros"
      it_should_behave_like "interpreter with continuations"
    end
    
    describe "with laziness" do
      let(:options) { {:unhygienic => true, :lazy => true} }
      it_should_behave_like "Scheme interpreter"
      it_should_behave_like "interpreter with unhygienic macros"
    end
  end
  
  describe "with continuations" do
    let(:options) { {:continuations => true} }
    it_should_behave_like "Scheme interpreter"
    it_should_behave_like "interpreter with hygienic macros"
    it_should_behave_like "interpreter with continuations"
  end
  
  describe "with laziness" do
    let(:options) { {:lazy => true} }
    it_should_behave_like "Scheme interpreter"
    it_should_behave_like "interpreter with hygienic macros"
  end
end

