deff w1(rc_1, env_1, f, y) = {
  valp v_4 = block-get(env_1, 1);
  valp v_5 = block-get(env_1, 2);
  defc rc_2(r_1) = {
    vall t_3 = 1;
    valp t_4 = r_1 - t_3;
    valp t_5 = v_4 >> t_3;
    valp t_6 = t_4 * t_5;
    valp a = t_6 + t_3;
    rc_1(a)
  };
  valp t_1 = v_5 + y;
  vall t_2 = 1;
  valp xy = t_1 - t_2;
  vall t_7 = 0;
  valp t_8 = block-get(f, t_7);
  t_8(rc_2, f, xy)
}
valp g = block-alloc-202(3);
valp t_29 = block-set(g, 0, w1);
valp t_27 = block-set(g, 1, z);
valp t_25 = block-set(g, 2, x);


deff w2(rc_3, env_2, x) = {
  valp v_6 = block-get(env_2, 1);
  vall t_11 = 1;
  valp t_12 = x - t_11;
  valp t_13 = v_6 >> t_11;
  valp t_14 = t_12 * t_13;
  valp xy = t_14 + t_11;
  rc_3(xy)
};
valp f = block-alloc-202(2);
valp t_23 = block-set(f, 0, w2);
valp t_21 = block-set(f, 1, y);


defc ci(r) = {
  vall c1 = 1;
  valp t1  = r >> c1;
  halt(t1)
}
vall x = 3;
vall y = 5;
vall z = 7;
valp t_19 = block-get(g, 0);
t_19(ci, g, f, y)