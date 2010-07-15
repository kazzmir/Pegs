puts(<<x + <<y)
qoobar
baz
x
more
crap
y

puts hello, world

<<-eos
    Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor 
    deserunt mollit anim id est laborum.
  eos

<<eos
    Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor 
    deserunt mollit anim id est laborum.
eos

Content.new(:value => <<eos)
    Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor 
    incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud 
    exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute 
    irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla 
    pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia
eos

Content.new(:value => <<eos
  Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor 
  incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud 
  exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute 
  irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla 
  pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia
eos
)
