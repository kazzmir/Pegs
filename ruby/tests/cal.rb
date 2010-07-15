
require 'date'

class Cal

  START =
    {
    'cn' => true,    
    'de' => 2342032, 
    'dk' => 2342032, 
    'es' => 2299161, 
    'fi' => 2361390, 
    'fr' => 2299227, 
    'gb' => 2361222, 
    'gr' => 2423868, 
    'hu' => 2301004, 
    'it' => 2299161, 
    'jp' => true,    
    'no' => 2342032, 
    'pl' => 2299161, 
    'pt' => 2299161, 
    'ru' => 2421639, 
    'se' => 2361390, 
    'us' => 2361222, 
    'os' => false,   
    'ns' => true     
  }

  DEFAULT_START = 'gb'

  def initialize
    opt_j; opt_m; opt_t; opt_y; opt_c
  end

  def opt_j(flag=false) @opt_j = flag end
  def opt_m(flag=false) @opt_m = flag end
  def opt_t(flag=false) @opt_t = flag end
  def opt_y(flag=false) @opt_y = flag end

  def opt_c(arg=DEFAULT_START) @start = START[arg] end

  def set_params
    @dw = if @opt_j then 3 else 2 end
    @mw = (@dw + 1) * 7 - 1
    @mn = if @opt_j then 2 else 3 end
    @tw = (@mw + 2) * @mn - 2
    @k  = if @opt_m then 1 else 0 end
    @da = if @opt_j then :yday else :mday end
  end

  def pict(y, m)
    d = (1..31).detect{|d| Date.valid_date?(y, m, d, @start)}
    fi = Date.new(y, m, d, @start)
    fi -= (fi.jd - @k + 1) % 7

    ve  = (fi..fi +  6).collect{|cu|
      %w(S M Tu W Th F S)[cu.wday]
    }
    ve += (fi..fi + 41).collect{|cu|
      if cu.mon == m then cu.send(@da) end.to_s
    }

    ve = ve.collect{|e| e.rjust(@dw)}

    gr = group(ve, 7)
    gr = trans(gr) if @opt_t
    ta = gr.collect{|xs| xs.join(' ')}

    ca = %w(January February March April May June July
	    August September October November December)[m - 1]
    ca = ca + ' ' + y.to_s if not @opt_y
    ca = ca.center(@mw)

    ta.unshift(ca)
  end

  def group(xs, n)
    (0..xs.size / n - 1).collect{|i| xs[i * n, n]}
  end

  def trans(xs)
    (0..xs[0].size - 1).collect{|i| xs.collect{|x| x[i]}}
  end

  def stack(xs)
    if xs.empty? then [] else xs[0] + stack(xs[1..-1]) end
  end

  def block(xs, n)
    stack(group(xs, n).collect{|ys| trans(ys).collect{|zs| zs.join('  ')}})
  end

  def unlines(xs)
    xs.collect{|x| x + "\n"}.join
  end

  def monthly(y, m)
    unlines(pict(y, m))
  end

  def addmon(y, m, n)
    y, m = (y * 12 + (m - 1) + n).divmod(12)
    return y, m + 1
  end

  def yearly(y)
    y.to_s.center(@tw) + "\n\n" +
      unlines(block((0..11).collect{|n| pict(*addmon(y, 1, n))}, @mn)) + "\n"
  end

  def print(y, m)
    set_params
    if @opt_y then yearly(y) else monthly(y, m) end
  end

end

if __FILE__ == $0

  require 'getopts'

  def usage
    warn 'usage: cal [-c iso3166] [-jmty] [[month] year]'
    exit 1
  end

  usage unless getopts('jmty', "c:#{Cal::DEFAULT_START}")

  y, m = ARGV.values_at(1, 0).compact.collect{|x| x.to_i}
  $OPT_y ||= (y and not m)

  to = Date.today
  y ||= to.year
  m ||= to.mon

  usage unless m >= 1 and m <= 12
  usage unless y >= -4712
  usage if Cal::START[$OPT_c].nil?

  cal = Cal.new

  cal.opt_j($OPT_j)
  cal.opt_m($OPT_m)
  cal.opt_t($OPT_t)
  cal.opt_y($OPT_y)
  cal.opt_c($OPT_c)

  print cal.print(y, m)

end
