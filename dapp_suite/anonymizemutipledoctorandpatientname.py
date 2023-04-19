import glob
text="Doctor"
textp="Patient name"
new_text="Doctor=abc"
new_textp="Patient name= aaa"
for file in glob.glob("C:/users/hp/doctorpatient/*.txt"):
  with open(file)as openfile:
    for line in openfile:
      if text in line:
        line=new_text
      if textp in line:
        line=new_textp
        print(line)
