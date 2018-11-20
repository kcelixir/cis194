defmodule CC do
  @spec to_digits(integer) :: [integer]
  def to_digits(num) do
    num
    |> to_digits_rev()
    |> Enum.reverse()
  end

  @spec to_digits_rev(integer) :: [integer]
  def to_digits_rev(num) when num <= 0 and is_integer(num) do
    []
  end

  def to_digits_rev(num) when is_integer(num) do
    [Integer.mod(num, 10) | num |> div(10) |> to_digits_rev()]
  end

  @spec double_every_other([integer]) :: [integer]
  def double_every_other(digits) do
    double = &(&1 * 2)
    digits
    |> mapl(&(Enum.map_every(&1, 2, double)))
  end

  @spec mapl([term], ([term] -> [term])) :: [term]
  def mapl(items, fun) do
    items
    |> Enum.reverse()
    |> fun.()
    |> Enum.reverse()
  end

  def validate(card) do
    0 ==
      card
      |> total()
      |> rem(10)
  end

  def total(card) do
    card
    |> to_digits()
    |> double_every_other()
    |> Enum.reduce(0, &sum_digits/2)
  end

  def sum_digits(integer, sum) do
    integer
    |> to_digits()
    |> Enum.sum()
    |> Kernel.+(sum)
  end
end

IO.inspect(CC.validate(4_012_888_888_881_882))
IO.inspect(CC.validate(4_012_888_888_881_881))

IO.inspect(CC.to_digits(1234))

IO.inspect(CC.double_every_other([8, 7, 6, 5]))
IO.inspect(CC.double_every_other([1, 2, 3]))
