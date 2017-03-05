var id = 100;
chrome.browserAction.onClicked.addListener(function() {
    var viewTabUrl = chrome.extension.getURL('moren.html?id=' + id++)
    var targetId = null;
    chrome.tabs.create({url: viewTabUrl}, function(tab) {
      targetId = tab.id;
    });
  });
