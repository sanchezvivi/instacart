
# dt_aisles -----
    # Rows: 134
    # Columns: 2
    # $ aisle_id <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,...
    # $ aisle    <chr> "prepared soups salads", "specialty ch...


# dt_departments -----
    # Rows: 21
    # Columns: 2
    # $ department_id <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11...
    # $ department    <chr> "frozen", "other", "bakery", "pro...


# dt_order_products__prior -----
    # Rows: 32,434,489
    # Columns: 4
    # $ order_id          <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 3,...
    # $ product_id        <dbl> 33120, 28985, 9327, 45918, 30...
    # $ add_to_cart_order <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 1,...
    # $ reordered         <dbl> 1, 1, 0, 1, 0, 1, 1, 1, 0, 1,...

# dt_order_products__train -----
    # Rows: 1,384,617
    # Columns: 4
    # $ order_id          <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 36, 3...
    # $ product_id        <dbl> 49302, 11109, 10246, 49683, 4...
    # $ add_to_cart_order <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 1, 2,...
    # $ reordered         <dbl> 1, 1, 0, 0, 1, 0, 0, 1, 0, 1,...


# dt_orders -----
    # Rows: 3,421,083
    # Columns: 7
    # $ order_id               <dbl> 2539329, 2398795, 473747...
    # $ user_id                <dbl> 1, 1, 1, 1, 1, 1, 1, 1, ...
    # $ eval_set               <chr> "prior", "prior", "prior...
    # $ order_number           <dbl> 1, 2, 3, 4, 5, 6, 7, 8, ...
    # $ order_dow              <dbl> 2, 3, 3, 4, 4, 2, 1, 1, ...
    # $ order_hour_of_day      <chr> "08", "07", "12", "07", ...
    # $ days_since_prior_order <dbl> NA, 15, 21, 29, 28, 19, ...


# dt_products -----
    # Rows: 49,688
    # Columns: 4
    # $ product_id    <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11...
    # $ product_name  <chr> "Chocolate Sandwich Cookies", "Al...
    # $ aisle_id      <dbl> 61, 104, 94, 38, 5, 11, 98, 116, ...
    # $ department_id <dbl> 19, 13, 7, 1, 13, 11, 7, 1, 16, 7...
