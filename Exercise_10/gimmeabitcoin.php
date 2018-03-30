<!DOCTYPE html PUBLIC
          "-//W3C//DTD XHTML 1.0 Transitional//EN"
          "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<?php
  if ("${_SERVER['QUERY_STRING']}" == "")
    $self = "${_SERVER['PHP_SELF']}";
  else
    $self = "${_SERVER['PHP_SELF']}?${_SERVER['QUERY_STRING']}";
  
  function find_rand_magic_num() {
    $rand_num = rand(0, 4242);
    $hashed_num = hash('sha256', $rand_num);
    return substr($hashed_num, 0, 4);
  }

  function bitcoin_amount($bitcoin) {
    $hex_amount = substr($bitcoin, 4, 4);
    $amount_in_cents = hexdec($hex_amount);
    return $amount_in_cents / 100;
  }

  session_start();

  if (!isset($_SESSION['amount'])) {
    $_SESSION['amount'] = 2000.00;
    $_SESSION['finished'] = false;
    $_SESSION['magic_num'] = find_rand_magic_num();
  }

  if (isset($_POST['answer'])) {
    $hashed_ans = hash('sha256', hex2bin($_POST['answer']));
    $hashed_2_ans = hash('sha256', hex2bin($hashed_ans));
    if (substr($hashed_2_ans, 0, 4) === $_SESSION['magic_num']) {
      $_SESSION['result'] = true;
      $_SESSION['res_amount'] = bitcoin_amount($hashed_2_ans);
      $_SESSION['amount'] -= $_SESSION['res_amount'];
      if ($_SESSION['amount'] <= 0) {
        $_SESSION['finished'] = true;
        $_SESSION['amount'] = 0;
      }
    }
    else {
      $_SESSION['result'] = false;
    }
    $_SESSION['magic_num'] = find_rand_magic_num();
  }
?>


<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
<title>Gimme a bitcoin!</title>
<style type="text/css">
<!--
body,td,th {
  font-family: Verdana, Arial, Helvetica, sans-serif;
  font-size: x-large;
  color: #CCCCCC;
}

body {
  background-color: #333399;
}

.title {
  font-family: "Courier New", Courier, monospace;
  font-weight: bold;
  font-size: 48px;
  color: #00FF66;
}

.question {color: #FFCC33}
.number {color: #FFFF33}
.md5sum {color: #FFCCFF}
.emph {color: #99ee99}
.alert {color: #ee77aa}

.right {
  color: #33FF66;
  font-weight: bold;
}
.wrong {
  color: #FF3366;
  font-weight: bold;
}

a:link {
  color: #CCFFFF;
}

a:visited {
  color: #CCFFFF;
}

input {
  background-color: #eeee66;
  color: #333399;
}

code {
  text-wrap: lowercase;   
  font-family: monospace;
  display: block;
  background-color: #66eeee;
  color: #993333;
  border: 1px solid black;
  padding: 8px;
  width: 95%;
  margin-top: 0.25em;
  margin-bottom: 0.25em;
}

input.wide {
  text-wrap: lowercase;
  font-family: monospace;
  font-size: x-large;
  color: #333333;
  border: 1px solid black;
  padding: 8px;
  width: 95%;
}

-->
</style>
</head>
<body>
<h1>Gimme a bitcoin!</h1>

<blockquote>
  <p>For the purpose of this exercise, <span class="emph">bitcoins</span>
  are 256-bit hexadecimal numbers, which, when hashed twice using SHA256,
  start with the 16-bit <span class="emph">magic code</span> given on this
  page. Notice that the magic code frequently changes.</p>
  <p>The 16-bits immediately after the magic code represent the bitcoin's
  <span class="emph">value</span>, given in euro cents.</p>
  <p>Bitcoins are represented in hexadecimal form, as strings of 64
    hexadecimal digits.<br/>
    Magic codes are represented as strings of 4 hexadecimal digits.</p>
  <p><span class="alert">Example:</span> If the magic code is 4217,
    the following string is a bitcoin worth 7.99 euro:
  <code>796fae438ebdc83ac3a4e8a071d71b1f0f0eace40d8a5b92bb64b1e9ed746066</code>
  </p>
</blockquote>

<p>I'd like to have 2000.00 euros, you still
  owe me <?php echo number_format($_SESSION['amount'], 2, ".", ""); ?>.</p>
<?php
  if (isset($_POST['answer'])) {
    if ($_SESSION['result'] && !$_SESSION['finished']) {
?>
      <p class="right">Right!  :-)</p>
      <p>You just gave me a bitcoin worth <?php echo number_format($_SESSION['res_amount'], 2, ".", ""); ?> euros. Thank you!</p>
      <hr />
      <form action="<?php echo $self; ?>" id="r" name="r" method="post">
        <input type="hidden" id="continue" name="continue" value="continue" />
        <input type="submit" name="again" id="again" value="Continue!" />
      </form>
    <?php
    }
    elseif ($_SESSION['result'] && $_SESSION['finished']) {
    ?>
      <p class="right">Right!  :-)</p>
      <p>You just gave me a bitcoin worth <?php echo number_format($_SESSION['res_amount'], 2, ".", ""); ?> euros. Thank you!</p>
      <hr />
      <!-- <p>It took you 584.249 seconds.</p> -->
      <form action="<?php echo $self; ?>" id="r" name="r" method="post">
        <input type="hidden" id="reset" name="reset" value="reset" />
        <input type="submit" name="again" id="again" value="Play again!" />
      </form>
    <?php
      unset($_SESSION['amount']);
    }
    else {
    ?>
      <p class="wrong">This is not a valid bitcoin!  :-(</p>
      <hr>
      <form action="<?php echo $self; ?>" id="r" name="r" method="post">
        <input id="continue" name="continue" value="continue" type="hidden">
        <input name="again" id="again" value="Continue!" type="submit">
      </form>
    <?php
    }
  }
  else {
    ?>
    <span class="question">The magic code is <?php echo $_SESSION['magic_num']; ?> </span></p>
    <form action="<?php echo $self; ?>" id="f" name="f" method="post">
      <input type="text" class="wide" name="answer" id="answer" /><br />
      <input type="submit" name="submit" id="submit" value="Submit!" />
      <input type="reset" value="Reset" />
    </form>
  <?php
  }
  ?>
</body>
</html>
