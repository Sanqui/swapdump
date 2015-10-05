out = ""
for line in open("swapdump.sym", "r"):
    if line.startswith("01:"):
        addr, rest = line.split()
        bank, offset = addr.split(":")
        offset = int(offset, 16)
        offset += 0x8000
        out += "00:{:04x} ".format(offset) + rest + "\n"
    else:
        out += line

with open("swapdump.sym", "w") as f:
    f.write(out)
