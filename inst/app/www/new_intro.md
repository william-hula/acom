This web-app implements the computer adaptive version of the Philadelphia Naming Test ([Roach et al., 1996](http://aphasiology.pitt.edu/215/1/24-09.pdf)) for clinical use. The 30-item computer adaptive Philadelphia Naming Test (PNT-CAT) can estimate the naming ability of people with aphasia with high reliability to the full 175-item PNT ([Fergadiotis, Hula, Swiderski, Lei, and Kellough, 2018](https://pubs.asha.org/doi/full/10.1044/2018_JSLHR-L-18-0344)). Rather than delivering all 175 items in order, The PNT-CAT chooses the next item based on how well an individual names previous items. Computer adaptive testing is based on item-response theory; see [Fergadiotis, Casilio, Hula, and Swiderski, 2021](https://www.thieme-connect.com/products/ejournals/abstract/10.1055/s-0041-1727252) and https://aswiderski.shinyapps.io/IRTapp/ for a primer on item-response theory and naming in aphasia.

This web-app allows Speech-Language Pathologists to:

1. Administer a 30-item PNT-CAT, 175-item PNT-CAT, 175-item Standard PNT. 
2. Re-administer a 30-item PNT-CAT or variable-length PNT-CAT to assess change in naming ability
3. Upload a scores from an offline PNT to get an ability estimate. 

For each option, users have the option of downloading performance data and a pdf-report of the results.

The code is open source and can be accessed on [Github](https://github.com/aphasia-apps/pnt). The web-app can also be installed locally and used without an internet connection. Refer to the github.com page for instructions for installation. Feedback and pull requests are welcome and encouraged. Please note that the app does not store any data beyond a single user-session. 

The development of this testing application was funded by National Institute on Deafness and Other Communication Disorders Awards R03DC014556 (PI: Fergadiotis) and R01DC018813 (MPIs: Fergadiotis & Hula), VA Rehabilitation Research & Development Career Development Award C7476W (PI: Hula), and the VA Pittsburgh Healthcare System Geriatric Research, Education, and Clinical Center. We would also like to acknowledge the support and assistance of Myrna Schwartz, Dan Mirman, Adelyn Brecher, Erica Middleton, Patrick Doyle, Malcolm McNeil, Christine Matthews, Angela Grzybowski, Brooke Swoyer, Maria Fenner, Michelle Gravier, Alyssa Autenreith, Emily Boss, Haley Dresang, Lauren Weerts, Hattie Olson, Kasey Graue, Chia-Ming Lei, Joann Silkes, Diane Kendall, the staff of the VA Pittsburgh Healthcare System Audiology and Speech Pathology Program.

To cite this app, please use: Robert Cavanaugh, Alexander Swiderski, G F and William D. Hula (2021). PNT.CAT: Philadelphia Naming Test - Computer Adaptive Test. R package version 0.0.0.9000. https://github.com/aphaisa-apps/pnt
