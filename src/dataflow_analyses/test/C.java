class C {

    int g;
    C f;

    int m(int x, C y) {
	int res = 0;
	int aux;
	while (y!=null) {
	    if (x==0) {
		res = y.f.g;
		aux = x+1;
	    } else {
		res = x;
		aux = x+1;
	    };
	    m1();
	}
	return res;
    }

    void m1() {}
}
