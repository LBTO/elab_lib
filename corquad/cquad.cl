procedure cquad (images)

file   images    {prompt="Image frame/list"}

struct *list1

begin
   string       input           # Image name from fscan
   file         infile1         # Temporary file list


   # get the filenames
   infile1 = mktemp ("tmp$cquad.")
   sections (images, option="fullname", >infile1)
   list1 = infile1

   # Loop through images
   while (fscan (list1, input) != EOF) {
        print ("Image ", input)
	corquad(input)
   }

   # delete temp file
   delete(infile1,ver-)

end
