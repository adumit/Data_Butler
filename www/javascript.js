$(document).ready(function(){
  $(document).on('click', 'button.advImputation', function(){
    $(".advImputationDiv").hide();
  });
  $(document).on('click', '#file1', function(){
    $("#doneMessage").hide();
  });
  $(document).on('click', 'button.finUpload', function(){
    $("#doneMessage").show();
  });
});