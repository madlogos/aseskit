#' @importFrom gWidgets2 gbasicdialog gvbox ggroup gframe gradio visible svalue
.tcltkSelectItem <- function(item.source, window.caption){
    # Select an item from a vector using gWidgets GUI
    # Args:
    #   item.source: vector of the item source
    #   window.caption: caption shown in the RGtk window
    # return:
    #   A list, output (logical) and dsource (vector of the item source)
    ## -----------Open a tcltk window------------
    op <- options(guiToolkit="tcltk")
    env0 <- environment()
    env0$dsource <- NULL
    env0$output <- FALSE
    window <- gbasicdialog(window.caption, do.buttons=TRUE, 
                           handler=function(h, ...){
        assign("dsource", enc2native(svalue(chkmap)), envir=env0)
        assign("output", TRUE, envir=env0)
    }, toolkit=guiToolkit(getOption("guiToolkit")))
    invisible(window$set_size(200, 200))
    box <- gvbox(cont=window)
    gg1 <- ggroup(cont=box)
    gg2 <- ggroup(cont=box, horizontal = TRUE)
    box1 <- gvbox(cont=gg1)
    frm1 <- gframe("Item Sources:", cont=box1)
    chkmap <- gradio(items=item.source, elected=1, index=TRUE, cont=frm1)
    visible(window)
	options(op)
    return(list(output=env0$output, items=env0$dsource))
    ## -----------------Close RGtk window---------------
}

.RGtkSelectItem <- function(item.source, window.caption){
    # Select an item from a vector using gWidgets GUI  (Deprecated!!!)
    # Args:
    #   item.source: vector of the item source
    #   window.caption: caption shown in the RGtk window
    # return:
    #   A list, output (logical) and dsource (vector of the item source)
    ## -----------Open a GUI window------------
    op <- options(guiToolkit="RGtk2")
    env0 <- environment()
    window <- gwindow(window.caption, width=200, height=200,
                      toolkit=guiToolkit(getOption("guiToolkit")))
    box <- gvbox(cont=window)
    addHandlerChanged(window, handler=function(...){
        gtkMainQuit()
    })
    gg1 <- ggroup(cont=box)
    gg2 <- ggroup(cont=box, horizontal = TRUE)
    box1 <- gvbox(cont=gg1)
    frm1 <- gframe("Item Sources:", cont=box1)
    chkmap <- gradio(items=item.source, elected=1, index=TRUE, cont=frm1)
    box21 <- gvbox(cont=gg2); box22 <- gvbox(cont=gg2)
    actOK <- gaction("  OK  ", "OK", handler=function(h, ...){
        assign("dsource", enc2native(svalue(chkmap)), envir=env0)
        assign("output", TRUE, envir=env0)
        dispose(window)
    })
    buttonOK <- gbutton(action=actOK, cont=box21)
    actCancel <- gaction("Cancel", "Cancel",
                         handler=function(h, ...){
                             assign("dsource", NULL, envir=env0)
                             assign("output", FALSE, envir=env0)
                             dispose(window)
                         })
    buttonCancel <- gbutton(action=actCancel, cont=box22)
    gtkMain()
	options(op)
    return(list(output=env0$output, items=env0$dsource))
    ## -----------------Close RGtk window---------------
}

#' @importFrom gWidgets2 gwindow gvbox addHandlerChanged ggroup gframe gaction gbutton gcombobox
.funSelVar <- function(vars){
    # Select a var from GUI
    # Args:
    #   vars: vector of vars
    # Return:
    #   A list, output (T/F) and the selected var
    env0 <- environment()
    window <- gwindow("Select the identifier variable", width=300, height=100)
    box <- gvbox(cont=window)
    addHandlerChanged(window, handler=function(...){
        gtkMainQuit()
    })
    gg1 <- ggroup(cont=box)
    gg2 <- ggroup(cont=box, horizontal = TRUE)
    box1 <- gvbox(cont=gg1)
    frm1 <- gframe("Identifier Variable:", cont=box1)
    chkmap <- gcombobox(items=vars, selected=-1, cont=frm1)
    size(chkmap) <- c(290, 25)
    box21 <- gvbox(cont=gg2)
    box22 <- gvbox(cont=gg2)
    actOK <- gaction("  OK  ", "OK", handler=function(h, ...){
        assign("dvar", enc2native(svalue(chkmap)), envir=env0)
        assign("output", TRUE, envir=env0)
        dispose(window)
    })
    buttonOK <- gbutton(action=actOK, cont=box21)
    actCancel <- gaction("Cancel", "Cancel",
                         handler=function(h,...){
        assign("dvar", NULL, envir=env0)
        assign("output", FALSE, envir=env0)
        dispose(window)
    })
    buttonCancel <- gbutton(action=actCancel, cont=box22)
    gtkMain()
    return(list(output=env0$output, vars=env0$dvar))
}

# --------- gui InputPwd------------

guiInputPwd <- function(prompt, caption, encrypt, guiClassObj, ...) {
    validClass <- c("Rstudioapi", "Gwidgets")
    if (! inherits(guiClassObj, validClass))
        warning("guiClassObj accepts ", paste(validClass, collapse=', '), 
                ". The default class will be applied.")
    UseMethod(".inputPwd", guiClassObj)
}

#' @export
#' @importFrom rstudioapi askForSecret isAvailable getVersion
.inputPwd.Rstudioapi <- function(prompt=NULL, caption=NULL, encrypt=TRUE, ...){
    if (! isAvailable() || getVersion() < '1.1.419')
        .inputPwd.Gwidgets(prompt, caption, encrypt, ...)
    pwdVal <- askForSecret("Password", message=ifnull(
        prompt, "Input the password: \n(DO NOT DISCLOSE THE PASSWORD TO OTHERS!)"), 
        title=ifnull(caption, "Enter the key"))
    if (is.null(pwdVal)) return(NULL)
    class(pwdVal) <- if (encrypt) "encrypted" else "plain"
    
    return(pwdVal)
}

#' @export
#' @importFrom gWidgets2 gbasicdialog gframe glabel ggroup gedit focus visible
.inputPwd.Gwidgets <- function(prompt=NULL, caption=NULL, encrypt=TRUE, ...){
    ## Based on code by Barry Rowlingson
    ## http://r.789695.n4.nabble.com/tkentry-that-exits-after-RETURN-tt854721.html#none
    
    if (getOption("guiToolkit") != "tcltk")
        return(.inputPwd.default(prompt, caption ,encrypt, ...))
    env0 <- environment()
    win <- gbasicdialog(ifnull(caption, "Enter the key"), handler=function(h, ...){
        pwdVal <- pwd$get_value()
        class(pwdVal) <- if (encrypt) "encrypted" else "plain"
        assign("pwdVal", pwdVal, envir=env0)
    }, toolkit=guiToolkit(getOption("guiToolkit")))
    size(win) <- c(100, 30)
    
    frame <- gframe(cont=win, horizontal=FALSE)
    box1 <- ggroup(horizontal=FALSE, cont=frame)
    msg <- glabel("DO NOT DISCLOSE THE PASSWORD TO OTHERS!", cont=box1)
    font(msg) <- list(family="helvetica", color="red")
    box2 <- ggroup(horizontal=FALSE, cont=frame)
    lbl <- glabel(ifnull(prompt, "Input the password:"), cont=box2)
    font(lbl) <- list(family="sans", scale="large")
    pwd <- gedit(cont=box2, width=25)
    visible(pwd) <- FALSE  # mask the input
    visible(win) <- TRUE
    focus(pwd) <- TRUE
    
    return(env0$pwdVal)
}

#' @export
.inputPwd.default <- .inputPwd.Rstudioapi

# ---------gui input Text----------------

guiInputText <- function(prompt, caption, default, guiClassObj, ...){
    validClass <- c("Rstudioapi", "Gwidgets", "WinGui")
    if (! inherits(guiClassObj, validClass))
        warning("guiClassObj accepts ", paste(validClass, collapse=", "),
                ". The default class will be applied.")
    UseMethod(".inputText", guiClassObj)
}

#' @export
#' @importFrom rstudioapi showPrompt isAvailable getVersion
.inputText.Rstudioapi <- function(prompt=NULL, caption=NULL, default="", ...){
    if (! isAvailable() || getVersion() < '1.1.67')
        .inputText.Gwidgets(prompt, caption, default, ...)
    txtVal <- showPrompt(ifnull(caption, "Input text"), message=ifnull(
        prompt, "Input a character:"), default=default)
    return(txtVal)
}

#' @export
#' @importFrom gWidgets2 gbasicdialog gframe glabel ggroup gedit focus visible
.inputText.Gwidgets <- function(prompt=NULL, caption=NULL, default="", ...){
    # tcltk
    if (getOption("guiToolkit") != "tcltk") 
        return(.inputText.default(prompt, caption, default, ...))
    env0 <- environment()
    win <- gbasicdialog(ifnull(caption, "Input text"), handler=function(h, ...){
        txtVal <- txt$get_value()
        assign("txtVal", txtVal, envir=env0)
    }, toolkit=guiToolkit(getOption("guiToolkit")))
    size(win) <- c(100, 30)
    
    box1 <- ggroup(horizontal=FALSE, cont=win)
    lbl <- glabel(ifnull(prompt, "Input the text:"), cont=box1)
    font(lbl) <- list(family="sans", scale="large")
    txt <- gedit(cont=box1, width=25)
    svalue(txt) <- default
    visible(win) <- TRUE
    focus(txt) <- TRUE
    
    return(env0$txtVal)
}

.inputText.WinGui <- function(prompt=NULL, caption=NULL, default="", ...){
    if (Sys.info()['sysname'] == 'Windows'){
        return(utils::winDialogString(message=ifnull(prompt, "Input text"), 
                                      default=default))
    }else{
        .inputText.default(prompt, caption, default, ...)
    }
}
    
.inputText.default <- .inputText.Rstudioapi


#' Input a character
#'
#' Input a character via GUI wizard. It returns nothing to the screen unless you 
#' assign the output to a variable.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param prompt character, the prompt label of the GUI window. Default NULL.
#' @param caption character, the caption of the GUI window. Default NULL.
#' @param default character, the default value for the inputbox. Default "".
#' @param gui 'Rstudioapi', 'Gwidgets' or 'WinGui'. Default 'Rstudioapi'. If 
#' 'Rstudioapi' and Rstudio version >= 1.1.67, then call 
#' \code{\link[rstudioapi]{showPrompt}}, or it calls \code{\link{winDialogString}} 
#' in Windows OS, otherwise uses a GUI wizard with gWidget2,
#' @param ... other arguments.
#' @return invisible
#' 
#' @export
#' @importFrom stringr str_to_title
#'
#' @seealso \code{\link[rstudioapi]{showPrompt}}  \code{\link[utils]{winDialogString}}
#' @examples
#' \dontrun{
#' x <- input_char(prompt="Input the api key", caption="API Key?")
#' }
#' 
inputChar <- function(prompt=NULL, caption=NULL, default="", 
                      gui=c("Rstudioapi", "Gwidgets", "WinGui"), ...){
    gui <- str_to_title(gui[[1]])
    gui <- match.arg(gui)
    guiClassObj <- structure(list, class=gui)
    out <- guiInputText(prompt, caption, default, guiClassObj)
    invisible(return(out))
}

#' @export
#' @rdname inputChar
input_char <- inputChar

# --------gui Dialog----------

guiDialog <- function(prompt, caption, guiClassObj,
                      type=c("ok", "okcancel", "yesno", "yesnocancel"), ...) {
    type <- match.arg(type)
    validClass <- c("Rstudioapi", "Gwidgets", "WinGui")
    if (! inherits(guiClassObj, validClass))
        warning("guiClassObj accepts ", paste(validClass, collapse=", "),
                ". The default class will be applied.")
    UseMethod(".guiDialog", guiClassObj)
}

#' @importFrom rstudioapi showQuestion isAvailable getVersion
.guiDialog.Rstudioapi <- function(prompt=NULL, caption=NULL, type, ...){
    if (! isAvailable() || getVersion() < '1.1.67')
        .guiDialog.Gwidgets(prompt, caption, type, ...)
    
    prompt <- ifnull(prompt, "Message")
    caption <- ifnull(caption, "Dialog")
    if (type == "ok"){
        out <- showDialog(caption, prompt)
    }else if (type == "okcancel"){
        out <- showQuestion(caption, prompt, "OK", "Cancel")
        if (! out) out <- NULL
    }else if (type %in%  c("yesno", "yesnocancel")){
        out <- showQuestion(caption, prompt, "Yes", "No")
    }
    return(out)
}

.guiDialog.Gwidgets <- function(prompt=NULL, caption=NULL, type, ...){
    # tcltk
    if (getOption("guiToolkit") != "tcltk") 
        return(.guiDialog.default(prompt, caption, type, ...))
    type <- match.arg(type)
    prompt <- ifnull(prompt, "Message")
    caption <- ifnull(caption, "Dialog")
    
    env0 <- environment()
    
    win <- gwindow(title=ifnull(caption, "Message"), 
                   visible=FALSE, toolkit=guiToolkit(getOption("guiToolkit")))
    size(win) <- c(100, 30)
    grp <- ggroup(horizontal=FALSE, cont=win)
    box1 <- gvbox(cont=grp)
    box2 <- ggroup(cont=grp)
    msg <- glabel(prompt, cont=box1)
    
    btnOK <- gbutton(if (type %in% c("ok", "okcancel")) "OK" else "Yes", cont=box2,
                     handler=function(h, ...){
                         assign("btnVal", TRUE, envir=env0)
                         dispose(win)
                     })
    if (type %in% c("yesno", "yesnocancel"))
        btnNo <- gbutton("No", cont=box2, handler=function(h, ...){
            assign("btnVal", FALSE, envir=env0)
            dispose(win)
        })
    if (type %in% c("okcancel", "yesnocancel"))
        btnCancel <- gbutton("Cancel", cont=box2, handler=function(h, ...){
            assign("btnVal", NULL, envir=env0)
            dispose(win)
        })
    visible(win) <- TRUE
    while (win$is_extant() && ! exists("btnVal", envir=env0)) Sys.sleep(0.25)
    return(env0$btnVal)
}

.guiDialog.WinGui <- function(prompt=NULL, caption=NULL, type, ...){
    prompt <- ifnull(prompt, "Message")
    caption <- ifnull(caption, "Dialog")
    if (Sys.info()['sysname'] == 'Windows'){
        out <- utils::winDialog(type=type, message=prompt)
        return(if (length(out) == 0) NULL else if (out %in% c("YES", "OK")) 
            TRUE else FALSE)
    }else{
        .guiDialog.default(prompt, caption, type, ...)
    }
}

.guiDialog.default <- if (Sys.info()['sysname'] == 'Windows') 
    .guiDialog.WinGui else .guiDialog.Rstudioapi 



