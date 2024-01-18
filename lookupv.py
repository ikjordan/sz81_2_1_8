x = 0
for a in range (2):
    xa = (15 * a) << 0
    for b in range (2):
        xb = (15 * b) << 8
        for c in range (2):
            xc = (15 * c) << 16
            for d in range (2):
                xd = (15 * d) << 24
                for e in range (2):
                    xe = (15 * e) << 32
                    for f in range (2):
                        xf = (15 * f) << 40
                        for g in range (2):
                            xg = (15 * g) << 48
                            for h in range (2):
                                xh = (15 * h) << 56
                                i = xa+xb+xc+xd+xe+xf+xg+xh
                                print(f"{i:#0{18}x},")
