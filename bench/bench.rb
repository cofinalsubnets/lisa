def clock
  Process.clock_gettime Process::CLOCK_MONOTONIC
end

def time
  t = clock
  yield
  clock - t
end

class Lang
  def nom; @nom end
  def cmd; @cmd end
  def initialize(nom, *cmd)
    @nom, @cmd = nom, cmd end
  def run(x, n, p=2)
    ts = []
    begin
      1.upto n do
        ts << time { raise unless system(*@cmd, x, [:err, :out] => "/dev/null") }
      end
      t = sprintf "%.#{p}fs", ts.sum/n
    rescue
      t = "failed"
    end
    puts sprintf("  %s %#{16-@nom.length}s", @nom, t)
  end end


class Bench < Hash
  A = {}
  PREF = "bench/"
  def nom; @nom end
  def initialize(nom)
    @nom = nom end
  def add(pt, f)
    self[pt] = PREF+f end
  def run(n, r=2)
    puts sprintf "benchmark %s", @nom
    puts "(avg of #{n} tests)" if n > 1
    each { |p, f| p.run f, n, r } end
  def self.mark(nom, n=1, p=2)
    yield (i = new nom)
    A[nom] = proc { i.run n, p } end
  def self.run
    A.keys.map(&A.method(:[])).filter(&:itself).each(&:[]) end
end

lisa = Lang.new "lisa", *ARGV.shift.split
ruby = Lang.new "ruby", "ruby"
mruby = Lang.new "mruby", "mruby"
chez = Lang.new "chez", "chez", "--script"
petite = Lang.new "petite", "petite", "--script"
guile = Lang.new "guile", "guile", "--auto-compile"
owl = Lang.new "owl", "ol"
pypy = Lang.new "pypy3", "pypy3"
hy = Lang.new "hy", "hy"
cpy = Lang.new "cpython", "python"
lua = Lang.new "lua", "lua"
luajit = Lang.new "luajit", "luajit"
ljoff = Lang.new '"" -joff', "luajit", "-joff"
sbcl = Lang.new "sbcl", "sbcl", "--script"
clisp = Lang.new "clisp", "clisp"
newlisp = Lang.new "newlisp", "newlisp", "-n"
ecl = Lang.new "ecl", "ecl", "--shell"
perl5 = Lang.new "perl5", "perl" # it's really slow
ghc = Lang.new "ghc", "runghc"
hugs = Lang.new "hugs", "runhugs"
node = Lang.new "node", "node"
deno = Lang.new "deno", "deno", "run"
pico = Lang.new "picolisp", "picolisp"
bash = Lang.new "bash", "bash"

Bench.mark("start/stop", 8, 4) do |bye|
  bye.add lisa, "bye.ls"
  bye.add luajit, "bye.lua"
  bye.add sbcl, "bye.lisp"
  bye.add petite, "bye.scm"
  bye.add chez, "bye.scm"
  bye.add pico, "bye.l"
  bye.add newlisp, "bye.lisp"
  bye.add node, "bye.js"
  bye.add deno, "bye.js"
  bye.add hy, "bye.hy"
  bye.add perl5, "bye.pl"
  bye.add ruby, "bye.rb"
  bye.add pypy, "bye.py"
  bye.add cpy, "bye.py"
  bye.add bash, "bye.sh"
  bye.add lua, "bye.lua"
end
#
Bench.mark("fib(32)") do |fib|
  fib.add lisa, "fib.ls"
  fib.add luajit, "fib.lua"
  fib.add ljoff, "fib.lua"
  fib.add sbcl, "fib.lisp"
  fib.add chez, "fib.scm"
  fib.add petite, "fib.scm"
  fib.add guile, "fib.scm"
  fib.add pico, "fib.l"
  fib.add newlisp, "fib.lsp"
  fib.add hy, "fib.hy"
  fib.add node, "fib.js"
  fib.add deno, "fib.js"
  fib.add ruby, "fib.rb"
  fib.add pypy, "fib.py"
  fib.add lua, "fib.lua"
  fib.add mruby, "fib.rb"
  fib.add cpy, "fib.py"
  fib.add owl, "fib.scm"
  fib.add ecl, "fib.lisp"
  fib.add clisp, "fib.lisp"
  fib.add perl5, "fib.pl"
  fib.add ghc, "fib.hs"
  fib.add hugs, "fib.hs"
end

Bench.mark("ack(3,9)") do |ack|
  ack.add lisa, "ack.ls"
  ack.add luajit, "ack.lua"
  ack.add ljoff, "ack.lua"
  ack.add sbcl, "ack.lisp"
  ack.add chez, "ack.scm"
  ack.add petite, "ack.scm"
  ack.add node, "ack.js"
  ack.add deno, "ack.js"
  ack.add petite, "ack.scm"
  ack.add guile, "ack.scm"
  ack.add ruby, "ack.rb"
  ack.add lua, "ack.lua"
  ack.add pico, "ack.l"
  ack.add newlisp, "ack.lsp"
  ack.add owl, "ack.scm"
  ack.add mruby, "ack.rb"
  ack.add ecl, "ack.lisp"
  ack.add clisp, "ack.lisp" # stack overflow
  ack.add pypy, "ack.py"
  ack.add hy, "ack.hy"
  ack.add cpy, "ack.py" # stack overflow
  ack.add perl5, "ack.pl"
  ack.add ghc, "ack.hs"
  ack.add hugs, "ack.hs"
end

Bench.run
