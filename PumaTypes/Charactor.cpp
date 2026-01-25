#include "pch.h"
#include "framework.h"
#include "Charactor.hpp"
#include "String.hpp" // needed for Charactor::ToString()

namespace Puma {
namespace Types
{
	namespace
	{
		constexpr std::uint8_t UTF8CharLengthLookup[32] =
		{
			1, 1, 1, 1, 1, 1, 1, 1,
			1, 1, 1, 1, 1, 1, 1, 1,
			1, 1, 1, 1, 1, 1, 1, 1,
			2, 2, 2, 2,
			3, 3,
			4,
			1
		};
	}

	Charactor::Charactor() noexcept
		: packedValue(0U)
	{
	}

	Charactor::Charactor(const Charactor& source) noexcept
		: packedValue(source.packedValue)
	{
	}

	Charactor::Charactor(const char* cstr) noexcept
		: packedValue(0U)
	{
		if (cstr == nullptr)
		{
			return;
		}

		const std::uint8_t first = static_cast<std::uint8_t>(cstr[0]);
		const std::uint8_t length = GetCharLength(first); // 1..4
		// Copy up to 4 bytes
		std::memcpy(codeUnits, cstr, length);
	}

	Charactor::~Charactor() noexcept = default;

	Charactor& Charactor::operator=(const Charactor& source) noexcept
	{
		if (this != &source)
		{
			packedValue = source.packedValue;
		}
		return *this;
	}

	String Charactor::ToString() const noexcept
	{
		char buffer[4] = { 0 };

		const std::uint8_t first = codeUnits[0];
		if (first == 0U)
		{
			// Empty character -> empty String
			return String();
		}

		const std::uint8_t length = GetCharLength(first); // 1..4
		// Copy up to 4 bytes
		std::memcpy(buffer, codeUnits, length);

		return String(buffer, length);
	}

	std::uint8_t Charactor::GetCharLength(std::uint8_t c) noexcept
	{
		return UTF8CharLengthLookup[c >> 3];
	}
} // namespace Types
} // namespace Puma