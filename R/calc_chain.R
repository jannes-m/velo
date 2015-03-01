#' @description Calculate the recommended number of chain links and the chain length in cm.
#' @param cs Length of the chain stay in mm.
#' @param chainring Number of teeth of the high gear chainring.
#' @param sprocket Number of teeth of the high gear sprocket.
#' @param idler Number of teeth of the idler roller, i.e. 10 or 11 (default: 10).
#' @export
#' @author Jannes Muenchow
#' @examples 
#' calc_chain(cs = 400, chainring = 52, sprocket = 28)

calc_chain <- function(cs = NULL, chainring = NULL, sprocket = NULL,
                       idler = 10) {
  cl <- round(0.157 * cs + 0.5 * chainring + 0.5 * sprocket + 2)
  # still somethin amiss, many functions use floor
  if (!isTRUE(all.equal(cl / 2, 0))) {
    cl <- cl + 1
  }  
  if (idler == 11) {
    cl <- cl + 2
  }
  cl_cm <- round(cl * 25.4 / 2 / 10)  
  list(cl, cl_cm)
}

# Kl=Kettenlenge(document.data.RiZ.value,document.data.KbZ.value,document.data.KeStLe.value);
# //alert("Kettenlänge ungerundet: "+(Kl));
# Kl=parseInt(Kl);
# //alert("Kettenlänge Integer: "+(Kl));
# if (Kl % 2 != 0) {Kl=Kl+1};
# Klcm=Kl*1.275;
# Klcm=dispnum(Klcm);
# alert("\n                                             Kettenlänge = "+(Kl)+" Glieder\n                                                Entspricht = "+(Klcm)+" cm"+"\n                           Das Ergebnis gilt für Schaltungsröllchen mit 10 Zähnen!"+"\nFür neueste Schaltungenröllchen mit 11 Zähnen muß die Kettenlänge um zwei Glieder verlängert werden!!!");
# }
# 
# function Kettenlenge(x1,x2,x3) {
#   x = dispnum(0.157*x3+x1/2+x2/2+2);
#   return(x);
# }
# function dispnum(x) {
#   x = Math.floor(x*10)/10; // give me two decimal places
#   return(x);
