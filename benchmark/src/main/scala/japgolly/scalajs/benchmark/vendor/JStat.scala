package japgolly.scalajs.benchmark.vendor

object JStat {

  object studentt {
    def inv(p: Double, dof: Int): Double = {
      var x = ibetainv(2 * Math.min(p, 1 - p), 0.5 * dof, 0.5);
      x = Math.sqrt(dof * (1 - x) / x);
      return if (p > 0.5) x else -x;
    }
  }

  def gammaln(x: Double): Double = {
    var j = 0;
    val cof = Array(
      76.18009172947146, -86.50532032941677, 24.01409824083091,
      -1.231739572450155, 0.1208650973866179e-2, -0.5395239384953e-5
    );
    var ser = 1.000000000190015;
    val xx = x
    var y = x
    var tmp = 0.0
    tmp = x + 5.5;
    tmp -= (xx + 0.5) * Math.log(tmp);
    while (j < 6) {
      y += 1
      ser += cof(j) / y;
      j += 1
    }
    return Math.log(2.5066282746310005 * ser / xx) - tmp;
  }

  // Returns the inverse of the incomplete beta function
  def ibetainv(p: Double, a: Double, b: Double): Double = {
    val EPS = 1e-8;
    val a1 = a - 1;
    val b1 = b - 1;
    var j = 0;
    var lna = 0.0
    var lnb = 0.0
    var pp = 0.0
    var t = 0.0
    var u = 0.0
    var err = 0.0
    var x = 0.0
    var al = 0.0
    var h = 0.0
    var w = 0.0
    var afac = 0.0
    if (p <= 0)
      return 0;
    if (p >= 1)
      return 1;
    if (a >= 1 && b >= 1) {
      pp = if (p < 0.5) p else 1 - p;
      t = Math.sqrt(-2 * Math.log(pp));
      x = (2.30753 + t * 0.27061) / (1 + t * (0.99229 + t * 0.04481)) - t;
      if (p < 0.5)
        x = -x;
      al = (x * x - 3) / 6;
      h = 2 / (1 / (2 * a - 1) + 1 / (2 * b - 1));
      w = (x * Math.sqrt(al + h) / h) - (1 / (2 * b - 1) - 1 / (2 * a - 1)) *
        (al + 5 / 6 - 2 / (3 * h));
      x = a / (a + b * Math.exp(2 * w));
    } else {
      lna = Math.log(a / (a + b));
      lnb = Math.log(b / (a + b));
      t = Math.exp(a * lna) / a;
      u = Math.exp(b * lnb) / b;
      w = t + u;
      if (p < t / w)
        x = Math.pow(a * w * p, 1 / a);
      else
        x = 1 - Math.pow(b * w * (1 - p), 1 / b);
    }
    afac = -gammaln(a) - gammaln(b) + gammaln(a + b);
    while (j < 10) {
      if (x == 0 || x == 1)
        return x;
      err = ibeta(x, a, b) - p;
      t = Math.exp(a1 * Math.log(x) + b1 * Math.log(1 - x) + afac);
      u = err / t;
      t = u / (1 - 0.5 * Math.min(1, u * (a1 / x - b1 / (1 - x))))
      x -= t
      if (x <= 0)
        x = 0.5 * (x + t);
      if (x >= 1)
        x = 0.5 * (x + t + 1);
      if (Math.abs(t) < EPS * x && j > 0)
        return x;
      j += 1
    }
    return x;
  };

  def ibeta(x: Double, a: Double, b: Double): Double = {
    // Factors in front of the continued fraction.
    val bt =
      if (x == 0 || x == 1) 0
      else
        Math.exp(
          gammaln(a + b) - gammaln(a) -
            gammaln(b) + a * Math.log(x) + b *
            Math.log(1 - x)
        );
    if (x < 0 || x > 1)
      return Double.NaN;
    if (x < (a + 1) / (a + b + 2))
      // Use continued fraction directly.
      return bt * betacf(x, a, b) / a;
    // else use continued fraction after making the symmetry transformation.
    return 1 - bt * betacf(1 - x, b, a) / b;
  };

  def betacf(x: Double, a: Double, b: Double): Double = {
    val fpmin = 1e-30;
    var m = 1;
    val qab = a + b;
    val qap = a + 1;
    val qam = a - 1;
    var c = 1.0;
    var d = 1 - qab * x / qap;
    var m2 = 0.0
    var aa = 0.0
    var del = 0.0
    var h = 0.0

    // These q's will be used in factors that occur in the coefficients
    if (Math.abs(d) < fpmin)
      d = fpmin;
    d = 1 / d;
    h = d;

    while (m <= 100) {
      m2 = 2 * m;
      aa = m * (b - m) * x / ((qam + m2) * (a + m2));
      // One step (the even one) of the recurrence
      d = 1 + aa * d;
      if (Math.abs(d) < fpmin)
        d = fpmin;
      c = 1 + aa / c;
      if (Math.abs(c) < fpmin)
        c = fpmin;
      d = 1 / d;
      h *= d * c;
      aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2));
      // Next step of the recurrence (the odd one)
      d = 1 + aa * d;
      if (Math.abs(d) < fpmin)
        d = fpmin;
      c = 1 + aa / c;
      if (Math.abs(c) < fpmin)
        c = fpmin;
      d = 1 / d;
      del = d * c;
      h *= del;
      if (Math.abs(del - 1.0) < 3e-7)
        return h;
      m += 1
    }

    return h;
  };

}
