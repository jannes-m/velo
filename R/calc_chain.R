#' @title Calculate chain links and chain length
#' @description Calculate the recommended number of chain links and the chain
#'   length in cm.
#' @param cs Length of the chain stay in mm.
#' @param chainring Number of teeth of the high gear chainring.
#' @param sprocket Number of teeth of the high gear sprocket.
#' @param idler Number of teeth of the idler roller, i.e. 10 or 11 (default:
#'   10).
#' @export
#' @author Jannes Muenchow
#' @details The number of links have to be even so that you can connect both 
#'   ends of the chain. That means if the formula yields an odd number of links,
#'   the function autommatically adds one additional link.
#' @examples 
#' calc_chain(cs = 400, chainring = 52, sprocket = 28)

calc_chain <- function(cs = NULL, chainring = NULL, sprocket = NULL,
                       idler = 10) {
  # test if all necessary arguments are specified
  args <- c("cs", "chainring", "sprocket")
  ind <- mapply(function(x) {
    is.null(get(x))
  }, as.list(args))
  if (any(ind)) {
    stop("Please specify also: ", paste(args[ind], collapse = ", "))
  }
  
  # test if chainring and sprocket are integers
  ind <- vapply(list(chainring, sprocket), function(x) {
    isTRUE(all.equal(x %% 2, 0))
  }, logical(1))
  if (!any(ind)) {
    stop("Chainring and sprocket must be integers")
  }
  
  # test if idler is 10 or 11
  if (!idler %in% c(10, 11)) {
    stop("The value of idler must be 10 or 11!")
  }
  
  # test if chainring > sprocket
  rat <- chainring / sprocket
  if (rat < 1.5) {
    message("The ratio of chainring and sprocket is rather small: ", rat)
  }
  
  # some functions in the Internet also use floor
  cl <- round(0.157 * cs + 0.5 * chainring + 0.5 * sprocket + 2)
  # check if cl is even, if not add 1 to make it even
  if (!isTRUE(all.equal(cl %% 2, 0))) {
    cl <- cl + 1
  }  
  if (idler == 11) {
    cl <- cl + 2
  }
  # one chain link corresponds to half an inch
  cl_cm <- round(cl * 2.54 / 2, 2)  
  data.frame("links" = cl, "length" = cl_cm)
}

# chain length for Fixies:
# http://www.machinehead-software.co.uk/bike/chain_length/chain_length_calculator.html


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
