def foo(n)
 true
end

def foo(a,b)
 true
end

def foo(a,b,c = 2)
 true
end

def foo(a,b,c = 2, *d)
 true
end

def foo(a,b,c = 2, *d, &e)
 true
end

def foo(c = 2, *d, &e)
 true
end

def foo(*d, &e)
 true
end

def foo(&e)
 true
end

def kconv(code = Kconv::EUC)
  Kconv.kconv(self, code, Kconv::AUTO)
end

lambda{|x,y,z|
	1
}
