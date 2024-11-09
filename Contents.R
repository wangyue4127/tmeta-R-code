# A novel robust meta-analysis model using the t distribution 
# for outlier accommodation and detection (tMeta).
# Version 1.0 (R2024) 06-Jun-2024

# In the /expr directory:
# Demo:
#   demo_tmeta             - Fitting tMeta on the mag dataset.

# Basic functions:
#   simu_1                     - Conducts a single experiment using the tMeta model.
#   tmetaplot                - Shows evolution of log-likelihood versus number of iterations and CPU time.

# In the /prog directory:
#   tmeta                      - Obtains the maximum likelihood estimates for the tMeta model.
#   metaini                    - Initializes the parameters for tMeta.

# References:
#   [1] Wang, Y., Zhao, J.H., Jiang, F., Shi, L., & Pan, J.X. (2024).
#       A novel robust meta-analysis model using the t distribution for outlier accommodation and detection.
#       School of Statistics and Mathematics, Yunnan University of Finance and Economics, Kunming 650221, China.
#       arXiv preprint https://arxiv.org/pdf/2401.02203

# Author(s):
#    Jianhua Zhao
#    Yue Wang      <ywang.ynufe@qq.com>

# Copyright:

#   A novel robust meta-analysis model using the t distribution for 
#   outlier accommodation and detection is
#   Copyright (C) 2024 by Jianhua Zhao and Yue Wang.

#   The software package is free software; you can redistribute it
#   and/or modify it under terms of GNU General Public License as
#   published by the Free Software Foundation; either version 2 of
#   the license, or any later version. For more details, see licenses
#   at http://www.gnu.org.

#   The software package is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#   See the GNU General Public License for more details.

#   As stated in the GNU General Public License, it is not possible to
#   include this software library or even parts of it in a proprietary
#   program without written permission from the owners of the copyright.
#   If you wish to obtain such permission, you can reach us by mail:

#      A novel robust meta-analysis model using the t distribution 
#      for outlier accommodation and detection
#      School of Statistics and Mathematics 
#      Yunnan University of Finance and Economics
#      Kunming, 650221
#      China

# and by e-mail:
#      ywang.ynufe@qq.com

# Please, if you find any bugs, contact the authors.

#   $Name:  $ $Revision: 1.0 $  $Date: 2024-06-07$