Public Class StorePage
    '=======================================================
    'Name: Li Zhang
    '
    'Date: 1/16/2020
    '
   
    'Description:
    '   Roblox Catalog is a Visual Basic recreation of Roblox's Current Catalog: https://www.roblox.com/catalog/?Category=0
    '   The Roblox Catalog allows users to buy virtual clothing to their in game characters in the game Roblox.
    '   To buy items in the Roblox Catalog, Robux must be purchased as currency to buy the items. 
    '   Each Item has its own price in Robux, Robux can be purchased on this application.
    '   Sizes can be chosen for each item and each product page has its own item photo with the price in Robux.
    'Usage:
    '   Robux: The currency used in this program instead of regular money.
    '   Catalog Tab: Displays two featured items that can be changed, additionaly, there are random items that can be generated again by the user.
    '   Clothing Tab: Displays a grid full of items that can be purchased by the user.
    '   Robux Tab: Allows users to purchase Robux with real life currency so they can purchase items in the store.
    '  
    'Features:
    '   Templated Product Pages: Additional product pages can be made easily by passing the image, price, and product name through a function.
    '   Multiple Pages: Users can browse through pages of the application by using the top menu bar.
    '   Fuzzy Search Implementation: Users can search for items in the store with ease.
    '   Purchase History / Cart: Users can see their previous purchases through the application.
    '   Robux Page: Users can purchase Robux on the Robux Panel.
    '   Backpage: Users can go to the previous page on product pages. 
    '   Error-free input: Users can input anything without throwing exceptions in the program.
    '   Eye-friendly colour palette.
    '
    '=======================================================

    '--- Globals Variables -- -

    Public Const TaxRate As Integer = 13 ' The tax rate should stay the same 
    Public RobuxBalance As Integer = 0 ' The user's current roblox balance in global scope (To be used by other functions without it being passed through)
    Public CurrentItemPrice As Integer = 0 ' Variable is used to store the current price of the item that the user has selected for further purchasing.
    Public LastPage As String = "HomePanel" ' String that stores the name of the last page visited (Used by backpage function so a backpage can be traced).

    '--- Global Arrays ---

    Public PageNames As New List(Of String) From 'Stores all the panels' names on the form so it can be accessed later on by other functions.
    {
        "HomePanel",
        "ProductPagePanel",
        "ClothingPanel",
        "RobuxPanel",
        "CartPanel",
        "AddedToCartPanel",
        "SearchResultsPanel"
    }
    Public PageBool As New List(Of Integer) From ' Boolean Array, 1 represents that the page is visible and 0 represents that the page is not visible.
    {1, 0, 0, 0, 0, 0, 0}
    Public ShopList As New List(Of String) From ' A global array that stores all the products' name that is used later on by functions to identify products.
    {
        "AdidasHoodie",
        "🖤Billie Eilish Crop🖤",
        "SUPREME SHIRT",
        "GUCCI SHIRT",
        "Brown Bear Hoodie",
        "MarshMellowHoodie💯",
        "Ice Adidas Hoodie",
        "GoldCheeseHoodie"
    }
    Public PriceList As New List(Of String) From ' In specified order, the price of per item in the ShopList array.
    {
       "10",
       "99",
       "99",
       "44",
       "32",
       "42",
       "1000",
       "21"
    }
    Public LocationList As New List(Of String) From ' In specified order, the location of each image in the ShopList array.
        {
        
        }

    '-- Form Functions -- 

    'Random Items Front Page Initiation:
    Private Sub StorePage_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Dim Ra = New Random 'Create new seed
        Dim FirstRandomNumber As Integer ' Store the first random number
        Dim SecondRandomNumber As Integer ' Store the second random number
        FirstRandomNumber = Ra.Next(1, ShopList.Count) ' Generate a number from 1 to the amount of products.
        SecondRandomNumber = FirstRandomNumber ' Set second number as first number to satisfy do until loop.
        Do Until SecondRandomNumber <> FirstRandomNumber ' Keep generating random number until second random number does not equal to first one.
            SecondRandomNumber = Ra.Next(1, ShopList.Count) ' Set second number as a random number.
        Loop
        ForYouPicture1.Image = Image.FromFile(LocationList(FirstRandomNumber - 1)) ' Retrieve the image of the first random product through the image indexing
        ForYouTitle1.Text = ShopList(FirstRandomNumber - 1) ' Get the name of the first random product by using it as an index in the product array.
        ForYouPrice1.Text = String.Concat(PriceList(FirstRandomNumber - 1), " ", " Robux") ' Retrieve the cost of the random item by indexing into the price array and then concating it with " Robux" to display currency.
        ForYouPicture2.Image = Image.FromFile(LocationList(SecondRandomNumber - 1)) ' Retrieve the image of the second random product through the image indexing.
        ForYouTitle2.Text = ShopList(SecondRandomNumber - 1) ' Get the name of the second random product by using it as an index in the product array.
        ForYouPrice2.Text = String.Concat(PriceList(SecondRandomNumber - 1), " ", " Robux") ' Retrieve the cost of the random item by indexing the cost array and then concating "Robux" at the end to display currency.
    End Sub

    '-- Predefined Functions -- 

    Private Sub UpdateCart(ByVal ProductName As String, ByVal Quantity As Integer, ByVal Price As Integer)
        ' Function Purpose: 
        ' To update the purchase history of the user and display it onto the "Cart" panel.
        ' Parameters: (Name of the Product, The quantity purchased of the product, The price of each product)
        For i = 0 To CartProductList.Items.Count - 1 ' For each item existing in the user's cart
            If CartProductList.Items(i).ToString = ProductName Then ' If the item already exists
                CartQuantityList.Items(i) = CartQuantityList.Items(i) + Quantity ' Update the quantity since the user purchased more of the product.
                CartTotalList.Items(i) = CartTotalList.Items(i) + Price ' Update the price of the additional same product purchased.
                CartNetTotal.Text = CartNetTotal.Text + Price ' Update the net total of all products.
                Return
            End If
        Next
        ' If the item does not exist in the player's purchase history / cart.
        CartProductList.Items.Add(ProductName) ' Add the product onto the user's cart.
        CartQuantityList.Items.Add(Quantity) ' Add the quantity purchased by user.
        CartTotalList.Items.Add(Price) ' Add the price of the item.
        CartNetTotal.Text = CartNetTotal.Text + Price ' Update the net total.
    End Sub

    Private Sub BackPage(ByVal PastPage As String)
        ' Function Purpose:
        ' Allows users to go to the previous page when on the Product Page.
        For i As Integer = 0 To PageBool.Count - 1 ' Go through the active page array
            If PageBool(i) = 1 Then ' If the page is active
                Me.Controls(PageNames(i)).Visible = False ' Make the page invisible
            End If
        Next
        For j As Integer = 0 To PageNames.Count - 1 ' Go through all the pages on the program
            If PageNames(j) = PastPage Then ' If the page is the last page
                Me.Controls(PageNames(j)).Visible = True ' Make the page visible.
            End If
        Next

    End Sub

    Private Sub LoadPage(ByVal Index As Integer, ByVal Title As String, ByVal Type As String)
        ' Function Purpose:
        ' Allows the store page to setup pages for each product without making multiple pages for each product.
        ' Parameters: (The index of the product in the product array, The title of the product, the type of page)
        If Type = "ProductPage" Then ' If Product Page is requested for product
            For i As Integer = 0 To PageBool.Count - 1 ' Go through all the active pages
                If PageBool(i) = 1 Then ' If the page is active then 
                    Me.Controls(PageNames(i)).Visible = False ' Make the page not visible
                End If
            Next
            PageBool(Index - 1) = 1 ' Set the current page active
            ProductPageTitle.Text = Title ' Setup the title of the product page as what is passed through the function.
            For j As Integer = 0 To ShopList.Count - 1 'Loop through entire product list
                If ShopList(j) = Title Then ' if the current item is the title
                    ProductPagePanelImage.Image = Image.FromFile(LocationList(j)) ' Make the picture in the template as the product requested photo 
                    ProductPagePrice.Text = String.Concat(PriceList(j), " ", "Robux") ' Retrieve the price of the item from the price list and concat "Robux" at the end to specify currency.
                    Exit For ' Break out of loop since product is already found.
                End If
            Next
            ProductPageRadioSizeL.Checked = False ' Turn all the sizes for clothes off
            ProductPageRadioSizeS.Checked = False
            ProductPageRadioSizeM.Checked = False
            ProductPagePanelWarning.Text = "" ' Set the warning text as nothing
            ProductPagePanelAddToCart.Text = "Add To Cart" ' Change the text to Add To Cart since nothing is purchased yet.
            ProductPagePanel.BringToFront() ' Bring the page to front.
            ProductPagePanel.Visible = True ' Make the page visible for the user
        End If
        If Type = "TempCart" Then ' If the page requested is the temporarily cart then
            PageBool(5) = 1 ' Set the temporarily cart page in the array as active
            ProductPagePanelWarn.Visible = False ' Disable the warning for now
            AddedToCartPanelPurchase.Text = "Buy Now" ' Set the button to display "Buy Now"
            PageBool(Index - 1) = 1 ' Make the current page active
            AddedToCartPanelProductTitle.Text = Title ' Display the title of the product through what is passed through in the function.
            AddedToCartPanelProductQuantity.Text = ProductPagePanelCount.Text ' Display the quantity that is requested by the user.
            Dim CurrentSelection As String = Title ' Store the current selection in a variable to be used later.
            For j As Integer = 0 To ShopList.Count - 1 ' Go through all the products in the store
                If ShopList(j) = CurrentSelection Then ' If the current item in the product array is the item passed through the function.
                    Try
                        AddedToCartPanelProductImage.Image = Image.FromFile(LocationList(j)) ' Try to display the image of the product through the product image array.
                        AddedToCartPanelProductPrice.Text = String.Concat(PriceList(j) * AddedToCartPanelProductQuantity.Text, " ", "Robux") ' Try to display the price of the item through the price list.
                        CurrentItemPrice = PriceList(j) * AddedToCartPanelProductQuantity.Text ' Try to display the quantity that is requested by the user.
                        ProductPagePanelWarn.Visible = False ' Make the warning label as false.
                        Exit For ' Break out of loop
                    Catch ex As Exception ' If the user gave wrong input for the quantity textbox
                        ProductPagePanelWarn.Visible = True ' Warn the user to enter a valid number.
                        Return ' Don't display the product page by breaking out of the function.
                    End Try
                End If
            Next
            AddedToCartPanel.BringToFront() ' Bring the page to front
            AddedToCartPanel.Visible = True ' Make the page visible for the user
        End If
        If Type = "MainPage" Then ' If the page panels are requested to change.
            For i As Integer = 0 To PageBool.Count - 1 ' Go through all the active page panels.
                If PageBool(i) = 1 Then ' If the page is active then
                    Me.Controls(PageNames(i)).Visible = False ' Make the page inactive
                    If PageNames(i) = "RobuxPanel" Then ' If the page active is the Roblox Panel.
                        RobuxMenuLabel.ForeColor = Color.FromArgb(255, 255, 255) ' Make the text white to show that the page is inactive to the user.
                    End If
                    If PageNames(i) = "ClothingPanel" Then ' If the page active is the clothing panel.
                        ClothingMenuLabel.ForeColor = Color.FromArgb(255, 255, 255) ' Make text white to show that the page is inactive
                    End If
                    If PageNames(i) = "CartPanel" Then ' If the cart panel is active then
                        CartMenuLabel.ForeColor = Color.FromArgb(255, 255, 255) ' Make the text white to show that the page is inactive to the user.
                    End If
                End If
            Next
            PageBool(Index - 1) = 1 ' Make the current page active in the boolean array.
            Me.Controls(PageNames(Index - 1)).Visible = True ' Make the page visible
            If PageNames(Index - 1) = "RobuxPanel" Then ' If the page requested is Roblox Panel
                RobuxMenuLabel.ForeColor = Color.FromArgb(132, 193, 255) ' Make the text blue to show that the Roblox Panel is active.
            End If
            If PageNames(Index - 1) = "ClothingPanel" Then ' If the page requested is the clothing panel.
                ClothingMenuLabel.ForeColor = Color.FromArgb(132, 193, 255) ' Make the text blue to show that the clothing panel is active.
            End If
            If PageNames(Index - 1) = "CartPanel" Then ' If the page requested is the cart panel.
                CartMenuLabel.ForeColor = Color.FromArgb(132, 193, 255) ' Make the text blue to show that the cart panel is active.
            End If
        End If
    End Sub

    '-- Form Functions --

    Private Sub FeaturedAdidasHoodieBuy_Click(sender As Object, e As EventArgs) Handles FeaturedAdidasHoodieBuy.Click ' If the featured Adidas Product is clicked
        LastPage = "HomePanel" ' Set the previous page as the Home Panel
        LoadPage(2, "AdidasHoodie", "ProductPage") ' Load the Product Page for the Product
    End Sub

    Private Sub ProductPagePanelBackArrow_Click(sender As Object, e As EventArgs) Handles ProductPagePanelBackArrow.Click ' If backpage is requested
        BackPage(LastPage) ' Run the backpage function.
    End Sub

    Private Sub FeaturedBillieElishBuy_Click(sender As Object, e As EventArgs) Handles FeaturedBillieElishBuy.Click ' If the featured product is clicked on.
        LastPage = "HomePanel" ' Set the previous page as the Home Panel
        LoadPage(2, "🖤Billie Eilish Crop🖤", "ProductPage") ' Load the product page for the product.
    End Sub

    Private Sub ProductPagePanelAddToCart_Click(sender As Object, e As EventArgs) Handles ProductPagePanelAddToCart.Click ' If the Add to cart is clicked.
        Dim IsSucess As Boolean = False ' Set Boolean as false to check if the purchase can go through.
        If ProductPageRadioSizeS.Checked Then ' If one of the following is checked, then the purchase can go through.
            IsSucess = True
            AddedToCartPanelProductSize.Text = "Small"
        End If
        If ProductPageRadioSizeM.Checked Then
            IsSucess = True
            AddedToCartPanelProductSize.Text = "Medium"
        End If
        If ProductPageRadioSizeL.Checked Then
            IsSucess = True
            AddedToCartPanelProductSize.Text = "Large"
        End If
        If Not IsSucess Then ' If purchase can not go through then it means that the user never selected a size.
            ProductPagePanelWarning.Text = "Please Select A Size!" ' Warn the user to select a size.
        Else
            ProductPagePanelWarning.Text = "" ' If the purchase is sucessful then dont warn the user
            AddedToCartPanel.BringToFront() ' Show the user the cart
            LoadPage(5, ProductPageTitle.Text, "TempCart") ' Load the cart page for user
        End If
    End Sub

    Private Sub AddedToCartPanelClose_Click(sender As Object, e As EventArgs) Handles AddedToCartPanelClose.Click ' If add to cart close button is clicked
        AddedToCartPanel.Visible = False 'Make the page not visible
        AddedToCartPanel.SendToBack() ' Send the page to the back
    End Sub

    Private Sub AddedToCartPanelPurchase_Click(sender As Object, e As EventArgs) Handles AddedToCartPanelPurchase.Click ' If the purchase button is clicked.
        If RobuxBalance - CurrentItemPrice >= 0 Then ' Check if the user has enough Robux to purchase item.
            RobuxBalance = RobuxBalance - CurrentItemPrice ' If they do then update their balance.
            RobuxCount.Text = RobuxBalance ' Display their updated Robux count
            AddedToCartPanelPurchase.Text = "Purchased!" ' Show that the product is purchased.
            Dim StrippedRobux = AddedToCartPanelProductPrice.Text.Replace(" Robux", "") ' Get the price of the product by stripping Robux off of the price from the page
            UpdateCart(AddedToCartPanelProductTitle.Text, AddedToCartPanelProductQuantity.Text, StrippedRobux) ' Add the item to the user's cart.
        Else
            AddedToCartPanelPurchase.Text = "Invalid Funds" ' The user does not have enough funds so display that the user does not have enough funds.
        End If
    End Sub

    Private Sub ConvertButton_Click(sender As Object, e As EventArgs) Handles ConvertButton.Click
        Dim ToCad As Double ' Declare ToCad to use later on
        Dim WithTax As Double ' Decalre Price with tax to use after later on.
        Try ' Try this
            RobuxPanelError.Visible = False ' Make the warning not visible. 
            Dim RequestedRobux As Integer = RobuxPanelUserRobuxInput.Text '  Store the requested robux inside this variable.
            ToCad = RequestedRobux / 100 ' Convert to CAD by dividing Robux by 100.
            WithTax = (ToCad * 0.13) + ToCad ' Store the with tax value in this variable.
            RobuxDetailsLabelRobuxCost.Text = RequestedRobux ' Update the details label Robux
            RobuxDetailsLabelCADCost.Text = WithTax.ToString("0.00") ' Update the details CAD label.
            RobuxPanelCADPrice.Text = ToCad.ToString("0.00") ' Update the CAD label without tax.
        Catch ex As Exception ' If there is invalid input
            RobuxPanelError.Visible = True ' Tell the user to input a valid number.
        End Try
    End Sub

    Private Sub ConfirmButton_Click(sender As Object, e As EventArgs) Handles ConfirmButton.Click ' Confirm the user's order
        If Not RobuxPanelError.Visible Then ' If no warning is displayed
            RobuxBalance = RobuxPanelUserRobuxInput.Text + RobuxBalance ' Update the user's Robux balance.
            RobuxCount.Text = String.Concat(RobuxBalance, " ", "Robux") ' Display the robux amount by concating Robux at the end to display currency.
            RobuxPanelUserRobuxInput.Text = "0" ' Set the conversion tool to all zeros.
            RobuxPanelCADPrice.Text = "0"
        End If
    End Sub



    Private Sub UpperbarTitle_Click(sender As Object, e As EventArgs) Handles UpperbarTitle.Click ' If the home page is selected
        RefreshRandom() ' Refresh the random items
        LoadPage(1, Nothing, "MainPage") ' Load the main page
    End Sub
    Private Sub ClothingMenuLabel_Click(sender As Object, e As EventArgs) Handles ClothingMenuLabel.Click ' If the clothing page is selected
        LoadPage(3, Nothing, "MainPage") ' Load the clothing page
    End Sub
    Private Sub RobuxMenuLabel_Click(sender As Object, e As EventArgs) Handles RobuxMenuLabel.Click ' If the robux menu is selected
        LoadPage(4, Nothing, "MainPage") ' Load the robux page
    End Sub

    '-- clothing page form functions -- 

    Private Sub ClothingBuy1_Click(sender As Object, e As EventArgs) Handles ClothingBuy1.Click ' If the first item is selected
        LastPage = "ClothingPanel" ' Store the previous page as clothing page
        LoadPage(2, ClothingTitle1.Text, "ProductPage") ' Display the product's page
    End Sub

    Private Sub ClothingBuy2_Click(sender As Object, e As EventArgs) Handles ClothingBuy2.Click ' If the second item is selected
        LastPage = "ClothingPanel" ' Store the previous page as clothing page.
        LoadPage(2, ClothingTitle2.Text, "ProductPage") ' Display the product's page
    End Sub

    Private Sub ClothingBuy3_Click(sender As Object, e As EventArgs) Handles ClothingBuy3.Click ' If the third item is selected
        LastPage = "ClothingPanel" ' Store the previous page as clothing page.
        LoadPage(2, ClothingTitle3.Text, "ProductPage") ' Display the product's page
    End Sub

    Private Sub ClothingBuy4_Click(sender As Object, e As EventArgs) Handles ClothingBuy4.Click ' If the fourth item is selected
        LastPage = "ClothingPanel" ' Store the previous page as clothing page. 
        LoadPage(2, ClothingTitle4.Text, "ProductPage") ' Display the product's page
    End Sub

    Private Sub ClothingBuy5_Click(sender As Object, e As EventArgs) Handles ClothingBuy5.Click ' If the fifth item is selected.
        LastPage = "ClothingPanel" ' Store the previous page as the clothing page.
        LoadPage(2, ClothingTitle5.Text, "ProductPage") ' Display the product's page
    End Sub

    Private Sub ClothingBuy6_Click(sender As Object, e As EventArgs) Handles ClothingBuy6.Click ' If the sixth item is selected
        LastPage = "ClothingPanel" ' Store the previou page as the clothing page
        LoadPage(2, ClothingTitle6.Text, "ProductPage") ' Display the product's page
    End Sub
    Private Sub RefreshRandom() ' Refresh random number same thing as the top one but in a function
        Dim Ra = New Random
        Dim FirstRandomNumber As Integer
        Dim SecondRandomNumber As Integer
        FirstRandomNumber = Ra.Next(1, ShopList.Count)
        SecondRandomNumber = FirstRandomNumber
        Do Until SecondRandomNumber <> FirstRandomNumber
            SecondRandomNumber = Ra.Next(1, ShopList.Count)
        Loop
        ForYouPicture1.Image = Image.FromFile(LocationList(FirstRandomNumber - 1))
        ForYouTitle1.Text = ShopList(FirstRandomNumber - 1)
        ForYouPrice1.Text = String.Concat(PriceList(FirstRandomNumber - 1), " ", " Robux")
        ForYouPicture2.Image = Image.FromFile(LocationList(SecondRandomNumber - 1))
        ForYouTitle2.Text = ShopList(SecondRandomNumber - 1)
        ForYouPrice2.Text = String.Concat(PriceList(SecondRandomNumber - 1), " ", " Robux")
    End Sub
    Private Sub RefreshButton_Click(sender As Object, e As EventArgs) Handles RefreshButton.Click ' If the refresh button is clicked
        RefreshRandom() 'Refresh the random items
    End Sub

    Private Sub ForYouBuy1_Click(sender As Object, e As EventArgs) Handles ForYouBuy1.Click ' If the random item is clicked
        LastPage = "HomePanel" ' Store the previous page as the home page
        LoadPage(2, ForYouTitle1.Text, "ProductPage") ' Display the product's page
    End Sub

    Private Sub ForYouBuy2_Click(sender As Object, e As EventArgs) Handles ForYouBuy2.Click ' If the second random item is selected
        LastPage = "HomePanel" ' Store the previous page as the home page
        LoadPage(2, ForYouTitle2.Text, "ProductPage") ' Display the product's page
    End Sub
    Sub SubmitSearch()
        'Fuzzy Search Implmentation using Least Common Subsequence
        'Implementation Explained:
        '---------------------------
        'Find the longest common subsequence between each store item and user's query and check if the user's query has at least N (user's query / W) letters in common.
        'Changing W will result in more specific searches and vise versa would result in less specific searches. 
        'Item Containing at least (User's Query/ 1.2) letters seems to be the in between from specific searches and broad searches.
        'After checking if the Item contains >= minimum letters required to be considered a selection, the item is appended to the listbox for the user to select. 

        Dim SearchQuery As String = MenuBarSearchBar.Text 'Get the user's query string
        Dim CurrentSelection As String ' Placeholder for SelectedProductName
        Dim OriginalSelection As String ' Keep Original None Modified Version of SelectedProductName
        If Not SearchQuery = "Search.." Then ' If the user actaully inputed a new string. 
            SearchQuery = SearchQuery.ToLower().Replace(" ", "") ' For the user's query strip all whitepace and make the string lowercase for more accurate string matching.
            SearchResults.Items.Clear() ' Clear the search result from previous searches.
            For n = 1 To ShopList.Count ' Loop through the entire Shop's item catalog.
                Dim LCS As Integer = 0 ' The result of the longest common subsequence between the user's query and the current item selected from the shop.
                Dim DP As Integer(,) = New Integer(150, 150) {} ' Intialize 150 by 150 matrix to store the result at each stage for memoization.
                OriginalSelection = ShopList(n - 1) ' Store the original string
                CurrentSelection = OriginalSelection.ToLower().Replace(" ", "") 'Strip all whitespace and make it all lowercased
                'Fill up from bottom up.
                For i = 0 To SearchQuery.Length
                    For j = 0 To CurrentSelection.Length
                        If i = 0 Or j = 0 Then ' If i is 0 or j is 0 the LCS is automatically 0.
                            DP(i, j) = 0 ' Store results as 0
                        ElseIf (SearchQuery(i - 1) = CurrentSelection(j - 1)) Then ' If the current character is equal to the query's current character
                            DP(i, j) = 1 + DP(i - 1, j - 1) ' Get the previous cell's solutions and add one.
                        Else ' If the current character does not equal to the query's current character
                            DP(i, j) = Math.Max(DP(i, j - 1), DP(i - 1, j)) ' Get the largest LCS from the previous cell and to this cell.
                        End If
                    Next
                Next
                LCS = DP(SearchQuery.Length, CurrentSelection.Length) ' Fetch the result from the final buildup point. 
                If LCS >= SearchQuery.Length / 1.2 Then 'Check if the longest common subsequence covers the minimum letters matched with query.
                    SearchResults.Items.Add(OriginalSelection) ' If the result passes, store the result in the listbox for the user to see.
                End If
            Next
            LoadPage(7, Nothing, "MainPage") ' Load the page up to be visible
            If SearchResults.Items.Count = 0 Then ' If the listbox is empty that means no searches were found.
                SearchResultsWarning.Visible = True ' Display "No matches found" warning to user.
            Else
                SearchResultsWarning.Visible = False ' If there are searches found, turn the visibility off of the "No Matches Found" warning.
            End If
        End If
    End Sub
    Private Sub SearchButton_Click(sender As Object, e As EventArgs) Handles SearchButton.Click ' If the search button is clicked run the search function.
        SubmitSearch()
    End Sub
    Private Sub MenuBarSearchBar_PreviewKeyDown(sender As Object, e As System.Windows.Forms.PreviewKeyDownEventArgs) Handles MenuBarSearchBar.PreviewKeyDown ' If a key is pressed down
        If e.KeyCode = Keys.Enter Then ' If the key is enter then the user wants to search.
            SubmitSearch() ' Run search function
        End If
    End Sub
    Private Sub SearchResults_DoubleClick(sender As Object, e As EventArgs) Handles SearchResults.SelectedIndexChanged ' If the user clicks the result cell
        If SearchResults.SelectedIndex >= 0 Then ' If the index of the item in the listbox is not 0 
            Dim SelectedItem As String = CStr((SearchResults.Items(SearchResults.SelectedIndex))) ' Turn the selected item into string.
            MenuBarSearchBar.Text = "Search.." ' Set the search bar to its default:
            LastPage = "SearchResultsPanel" ' Set the last page as the search results panel.
            LoadPage(2, SelectedItem, "ProductPage") ' Load the product page for the item
        End If
    End Sub

    Private Sub CartMenuLabel_Click(sender As Object, e As EventArgs) Handles CartMenuLabel.Click ' If the cart menu is selected
        LoadPage(5, Nothing, "MainPage") ' Display the page
        If CartProductList.Items.Count = 0 Then ' If there are no items
            CartProductWarning.Visible = True ' Display to the user that they have no items in their cart
        Else
            CartProductWarning.Visible = False ' Disable the message
        End If
    End Sub

    Private Sub CartPanelItemInsurance_CheckedChanged(sender As Object, e As EventArgs) Handles CartPanelItemInsurance.CheckedChanged ' If insurrance is selected
        If CartPanelItemInsurance.Checked Then ' If it is checked
            CartNetTotal.Text = CartNetTotal.Text + 30 ' Add 30 robux to their net total
        Else
            CartNetTotal.Text = CartNetTotal.Text - 30 ' If it is unchecked subtract 30 from their net total.
        End If
    End Sub

End Class
