//
//  MainMenuController.swift
//  Digipostarkiv
//
//  Created by Frode on 19/01/15.
//  Copyright (c) 2015 Nerbraten. All rights reserved.
//

import Foundation
import Cocoa

class MenuController: NSObject {
    
    @IBOutlet weak var statusMenu: NSMenu!
    var statusItem: NSStatusItem?
    
    @IBOutlet weak var loginWindowController: LoginWindowController!
    
    override func awakeFromNib() {
        statusItem = NSStatusBar.systemStatusBar().statusItemWithLength(20.0)
        statusItem!.menu = statusMenu
        let statusImageSize = NSSize(width: 15.2, height: 20.0)
        let statusImage = NSImage(named: "digipost-black.png")
        statusImage?.size = statusImageSize
        let statusImageActive = NSImage(named: "digipost-red.png")
        statusImageActive?.size = statusImageSize
        let altStatusImage = NSImage(named: "digipost-white.png")
        altStatusImage?.size = statusImageSize
        statusItem!.image = statusImage
        statusItem!.alternateImage = altStatusImage
        statusItem!.highlightMode = true
    }
    
    @IBAction func exitApp(sender: NSMenu) {
        hs_exit()
        NSApplication.sharedApplication().terminate(self)
    }
    
    @IBAction func openInFinder(sender: NSMenu) {
        let homePath = NSHomeDirectory().stringByAppendingString("/Digipostarkiv")
        NSWorkspace.sharedWorkspace().openFile(homePath)
    }
    
    @IBAction func manualSync(sender: NSMenu) {
    }
    
    @IBAction func logIn(sender: NSMenu) {
        loginWindowController.login()
    }
    
    @IBAction func logOut(sender: NSMenu) {
        loginWindowController.logout()
    }
}